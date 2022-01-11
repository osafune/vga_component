-- ===================================================================
-- TITLE : VGA Controller / AvalonMM Burst Master
--
--     DESIGN : S.OSAFUNE (J-7SYSTEM Works)
--     DATE   : 2010/11/20 -> 2010/12/12
--            : 2010/12/27 (FIXED)
--
--     UPDATE : 2011/06/25 modify overrun enable condition
--              2021/12/06 removed overrun, video_dither
--              2022/01/02 add RGB/YUV output format option
--
-- ===================================================================
-- *******************************************************************
--   Copyright (C) 2010-2011, J-7SYSTEM Works.  All rights Reserved.
--
-- * This module is a free sourcecode and there is NO WARRANTY.
-- * No restriction on use. You can use, modify and redistribute it
--   for personal, non-profit or commercial products UNDER YOUR
--   RESPONSIBILITY.
-- * Redistributions of source code must retain the above copyright
--   notice.
-- *******************************************************************

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity vga_avm is
	generic (
--		PIXEL_COLORORDER	: string := "RGB565";
--		PIXEL_COLORORDER	: string := "RGB555";
--		PIXEL_COLORORDER	: string := "RGB444";
		PIXEL_COLORORDER	: string := "YUV422";
		BURSTCYCLE			: integer := 320;
		LINEOFFSETBYTES		: integer := 1024*2
	);
	port (
		----- AvalonMMクロック信号 -----------
		csi_m1_reset		: in  std_logic;
		csi_m1_clk			: in  std_logic;

		----- AvalonMMマスタ信号 -----------
		avm_m1_address		: out std_logic_vector(31 downto 0);
		avm_m1_waitrequest	: in  std_logic;
		avm_m1_burstcount	: out std_logic_vector(9 downto 0);

		avm_m1_read			: out std_logic;
		avm_m1_readdata		: in  std_logic_vector(31 downto 0);
		avm_m1_readdatavalid: in  std_logic;

		----- 外部信号 -----------
		framebuff_addr		: in  std_logic_vector(31 downto 0);
		framestart			: in  std_logic;
		linestart			: in  std_logic;
		ready				: out std_logic;

		video_clk			: in  std_logic;		-- typ 25MHz
		video_active		: in  std_logic;
		video_rout			: out std_logic_vector(7 downto 0);
		video_gout			: out std_logic_vector(7 downto 0);
		video_bout			: out std_logic_vector(7 downto 0);
		video_pixelvalid	: out std_logic
	);
end vga_avm;

architecture RTL of vga_avm is

	type BUS_STATE is (IDLE,
						READ_ISSUE,DATA_READ,READ_DONE,
						WRITE_ISSUE,WRITE_DONE);
	signal avm_state : BUS_STATE;
	signal datacount	: integer range 0 to BURSTCYCLE;
	signal topaddr_reg	: std_logic_vector(31 downto 0);
	signal lineoffs_reg	: std_logic_vector(31 downto 0);
	signal addr_reg		: std_logic_vector(31 downto 2);
	signal read_reg		: std_logic;
	signal write_reg	: std_logic;

	signal pixelcount_reg	: std_logic_vector(8 downto 0);

	signal readdata_sig			: std_logic_vector(31 downto 0);
	signal readdatavalid_sig	: std_logic;
	signal pixeladdr_reg		: std_logic_vector(9 downto 0);
	signal pixeldata_sig		: std_logic_vector(15 downto 0);
	signal valid_reg			: std_logic_vector(3 downto 0);

	signal pixel_r_sig			: std_logic_vector(7 downto 0);
	signal pixel_g_sig			: std_logic_vector(7 downto 0);
	signal pixel_b_sig			: std_logic_vector(7 downto 0);
	signal pixellatch_sig		: std_logic;
	signal rout_reg				: std_logic_vector(7 downto 0);
	signal gout_reg				: std_logic_vector(7 downto 0);
	signal bout_reg				: std_logic_vector(7 downto 0);
	signal outvalid_reg			: std_logic;

	component vga_linebuffer
	PORT
	(
		data		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		rdaddress	: IN STD_LOGIC_VECTOR (9 DOWNTO 0);
		rdclock		: IN STD_LOGIC ;
		wraddress	: IN STD_LOGIC_VECTOR (8 DOWNTO 0);
		wrclock		: IN STD_LOGIC  := '1';
		wren		: IN STD_LOGIC  := '0';
		q			: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
	);
	end component;

	component vga_yvu2rgb
	port(
		reset		: in  std_logic;
		clk			: in  std_logic;

		pixelvalid	: in  std_logic;
		y_data		: in  std_logic_vector(7 downto 0);
		uv_data		: in  std_logic_vector(7 downto 0);

		r_data		: out std_logic_vector(7 downto 0);
		g_data		: out std_logic_vector(7 downto 0);
		b_data		: out std_logic_vector(7 downto 0)
	);
	end component;

begin

	-- ステータス＆エラーチェック 

	ready   <= '1' when (avm_state = IDLE) else '0';



	-- AvalonMMバーストマスタ・ステートマシン 

	avm_m1_address    <= addr_reg & "00";
	avm_m1_burstcount <= CONV_STD_LOGIC_VECTOR(BURSTCYCLE, 10);
	avm_m1_read       <= read_reg;
	readdata_sig      <= avm_m1_readdata;
	readdatavalid_sig <= avm_m1_readdatavalid when (avm_state=DATA_READ) else '0';

	process (csi_m1_clk, csi_m1_reset) begin
		if (csi_m1_reset = '1') then
			avm_state <= IDLE;
			datacount <= 0;
			addr_reg  <= (others=>'0');
			read_reg  <= '0';
			write_reg <= '0';

			topaddr_reg  <= (others=>'0');
			lineoffs_reg <= (others=>'0');

		elsif rising_edge(csi_m1_clk) then

			case avm_state is
			when IDLE =>
				if (linestart = '1') then
					avm_state <= READ_ISSUE;
					addr_reg  <= topaddr_reg(31 downto 2) + lineoffs_reg(31 downto 2);
					read_reg  <= '1';
					datacount <= 0;
				end if;

			when READ_ISSUE =>
				if (avm_m1_waitrequest = '0') then
					avm_state <= DATA_READ;
					read_reg  <= '0';
				end if;

			when DATA_READ =>
				if (avm_m1_readdatavalid = '1') then
					if (datacount = BURSTCYCLE-1) then
						avm_state <= IDLE;
					end if;

					datacount <= datacount + 1;
				end if;

			when others=>
			end case;


			topaddr_reg <= framebuff_addr;

			if (framestart = '1') then
				lineoffs_reg <= (others=>'0');
			elsif (avm_state = IDLE and linestart = '1') then
				lineoffs_reg <= lineoffs_reg + CONV_STD_LOGIC_VECTOR(LINEOFFSETBYTES, 32);
			end if;

		end if;
	end process;



	-- ラインバッファメモリ 

	process (video_clk) begin
		if rising_edge(video_clk) then
			if (video_active = '0') then
				pixeladdr_reg <= (others=>'0');
			else
				pixeladdr_reg <= pixeladdr_reg + '1';
			end if;
		end if;
	end process;

	u0 : vga_linebuffer
	PORT MAP (
		wrclock		=> csi_m1_clk,
		wraddress	=> CONV_STD_LOGIC_VECTOR(datacount, 9),
		data		=> readdata_sig,
		wren		=> readdatavalid_sig,

		rdclock	 	=> video_clk,
		rdaddress	=> pixeladdr_reg,
		q			=> pixeldata_sig
	);


	-- ピクセルフォーマット変換 

GEN_RGB565 : if (PIXEL_COLORORDER = "RGB565") generate
	pixel_r_sig <= pixeldata_sig(15 downto 11) & pixeldata_sig(15 downto 13);
	pixel_g_sig <= pixeldata_sig(10 downto  5) & pixeldata_sig(10 downto  9);
	pixel_b_sig <= pixeldata_sig( 4 downto  0) & pixeldata_sig( 4 downto  2);
	pixellatch_sig <= valid_reg(1);
end generate;

GEN_RGB555 : if (PIXEL_COLORORDER = "RGB555") generate
	pixel_r_sig <= pixeldata_sig(14 downto 10) & pixeldata_sig(14 downto 12);
	pixel_g_sig <= pixeldata_sig( 9 downto  5) & pixeldata_sig( 9 downto  7);
	pixel_b_sig <= pixeldata_sig( 4 downto  0) & pixeldata_sig( 4 downto  2);
	pixellatch_sig <= valid_reg(1);
end generate;

GEN_RGB444 : if (PIXEL_COLORORDER = "RGB444") generate
	pixel_r_sig <= pixeldata_sig(11 downto  8) & pixeldata_sig(11 downto  8);
	pixel_g_sig <= pixeldata_sig( 7 downto  4) & pixeldata_sig( 7 downto  4);
	pixel_b_sig <= pixeldata_sig( 3 downto  0) & pixeldata_sig( 3 downto  0);
	pixellatch_sig <= valid_reg(1);
end generate;

GEN_YUV422 : if (PIXEL_COLORORDER = "YUV422") generate
	u_yvu2rgb : vga_yvu2rgb
	port map (
		reset		=> '0',
		clk			=> video_clk,
		pixelvalid	=> valid_reg(1),
		y_data		=> pixeldata_sig(15 downto 8),
		uv_data		=> pixeldata_sig(7 downto 0),
		r_data		=> pixel_r_sig,
		g_data		=> pixel_g_sig,
		b_data		=> pixel_b_sig
	);
	pixellatch_sig <= valid_reg(3);
end generate;


	-- 出力データラッチ 

	process (video_clk) begin
		if rising_edge(video_clk) then
			valid_reg <= valid_reg(valid_reg'left-1 downto 0) & video_active;

			if (pixellatch_sig = '1') then
				rout_reg <= pixel_r_sig;
				gout_reg <= pixel_g_sig;
				bout_reg <= pixel_b_sig;
			else
				rout_reg <= (others=>'0');
				gout_reg <= (others=>'0');
				bout_reg <= (others=>'0');
			end if;

			outvalid_reg <= pixellatch_sig;

		end if;
	end process;

	video_rout <= rout_reg;
	video_gout <= gout_reg;
	video_bout <= bout_reg;
	video_pixelvalid <= outvalid_reg;




end RTL;
