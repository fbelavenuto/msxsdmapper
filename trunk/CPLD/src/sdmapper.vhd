--
-- Projeto MSX SD Mapper
--
-- Copyright (c) 2014
-- Fabio Belavenuto

-- This documentation describes Open Hardware and is licensed under the CERN OHL v. 1.1.
-- You may redistribute and modify this documentation under the terms of the
-- CERN OHL v.1.1. (http://ohwr.org/cernohl). This documentation is distributed
-- WITHOUT ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING OF MERCHANTABILITY,
-- SATISFACTORY QUALITY AND FITNESS FOR A PARTICULAR PURPOSE.
-- Please see the CERN OHL v.1.1 for applicable conditions

-- Modulo TOP - implementa o controle da Megarom ASCII16 para o Nextor

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;			-- AND_REDUCE

entity sdmapper is
	port(
		clock				: in    std_logic;
		reset_n			: in    std_logic;
		dis_mapper		: in    std_logic;

		-- CPU interface
		addr_bus			: in    std_logic_vector(15 downto 0);
		data_bus			: inout std_logic_vector(7 downto 0);
		wr_n				: in    std_logic;
		rd_n				: in    std_logic;
		iorq_n			: in    std_logic;
		m1_n				: in    std_logic;
		sltsl_n			: in    std_logic;

		-- ROM interface
		rom_a				: out   std_logic_vector(16 downto 14);
		rom_cs			: out   std_logic;
		rom_we			: out   std_logic;

		-- RAM interface
		ram_a				: out   std_logic_vector(18 downto 14);
		ram_cs			: out   std_logic;

		-- SD card interface
		sd_cs				: out   std_logic_vector(1 downto 0);	-- Saida Chip Select para os cartoes
		sd_sclk			: out   std_logic;							-- Saida SCK
		sd_mosi			: out   std_logic;							-- Master Out Slave In
		sd_miso			: in    std_logic;							-- Master In Slave Out
		sd_writeprot	: in    std_logic_vector(1 downto 0);	-- 0 = cartao protegido contra escrita
		sd_inserted		: in    std_logic_vector(1 downto 0)	-- 0 = cartao inserido
	);

end sdmapper;

architecture Behavioral of sdmapper is

	signal io_cs	  		: std_logic;
	signal b7_en			: std_logic;
	signal iomapper		: std_logic;
	signal ffff				: std_logic;
	signal sltsl_c			: std_logic;
	signal slt_exp_n		: std_logic_vector(3 downto 0);
	signal sltsl_rom		: std_logic;
	signal sltsl_ram		: std_logic;

	-- flash
	signal mrnextor		: std_logic;
	signal mr_addr			: std_logic_vector(2 downto 0);
	signal rom_wr_en		: std_logic;
	signal rom_adhi		: std_logic_vector(2 downto 0);
	signal rcewr			: std_logic;


begin

	-- Porta SPI
	portaspi: entity work.sd
	port map (
		clock				=> clock,
		reset_n			=> reset_n,
		dis_mapper		=> dis_mapper,
		-- CPU interface
		addr_bus			=> addr_bus(7 downto 0),
		data_bus			=> data_bus,
		wr_n				=> wr_n,
		rd_n				=> rd_n,
		iorq_n			=> iorq_n,
		m1_n				=> m1_n,
		-- SD card interface
		sd_cs				=> sd_cs,
		sd_sclk			=> sd_sclk,
		sd_mosi			=> sd_mosi,
		sd_miso			=> sd_miso,
		sd_writeprot	=> sd_writeprot,
		sd_inserted		=> sd_inserted
	);

	-- Expansor de slot
	exp: entity work.exp_slot
	port map (
		reset_n		=> reset_n,
		sltsl_n		=> sltsl_c,
		cpu_rd_n		=> rd_n,
		cpu_wr_n		=> wr_n,
		ffff			=> ffff,
		cpu_a			=> addr_bus(15 downto 14),
		cpu_d			=> data_bus,
		exp_n			=> slt_exp_n
	);

	-- Mapper
	mapp: entity work.mapper
	port map (
		reset_n		=> reset_n,
		cpu_a			=> addr_bus,
		cpu_d			=> data_bus,
		ioFC_FF		=> iomapper,
		cpu_rd_n		=> rd_n,
		cpu_wr_n		=> wr_n,
		sltsl_n		=> sltsl_ram,
		sram_ma		=> ram_a,
		sram_cs_n	=> ram_cs
	);

	-- Glue Logic

	-- FFFF eh 1 quando todos os bits do barramento de enderecos for 1 (usado no expansor de slots)
	ffff    <= AND_REDUCE(addr_bus);

	-- Slot Selects
	sltsl_c		<= sltsl_n      when dis_mapper = '1' else '1';
	sltsl_rom	<= slt_exp_n(1) when dis_mapper = '1' else sltsl_n;
	sltsl_ram	<= slt_exp_n(3) when dis_mapper = '1' else '1';

	-- Enable portas I/O
	io_cs	<= not iorq_n and m1_n;

	b7_en		<= '1' when io_cs = '1' and addr_bus(7 downto 0) = X"B7" and wr_n = '0'	else '0';	-- Acesso I/O escrita porta $B7
	iomapper	<= '1' when io_cs = '1' and addr_bus(7 downto 2) = "111111"					else '0';	-- Acesso I/O portas $FC a $FF

	-- Escrita porta $B7
	process (reset_n, b7_en)
	begin
		if reset_n = '0' then
			rom_wr_en <= '0';
			rom_adhi  <= (others => '0');
		elsif rising_edge(b7_en) then
			rom_wr_en	<= data_bus(7);
			rom_adhi		<= data_bus(2 downto 0);
		end if;
	end process;

	-- Controle da MEGAROM ASCII16 do Nextor
	-- Gravacao no endereco $6000 chaveia o banco somente se nao estiver no modo gravacao da flash
	mrnextor <= '1' when wr_n = '0' and sltsl_rom = '0' and addr_bus(15 downto 12) = "0110" and rom_wr_en = '0'	else '0';

	process (reset_n, mrnextor)
	begin
		if reset_n = '0' then 
			mr_addr <= (OTHERS => '0');
		elsif rising_edge(mrnextor) then
			mr_addr <= data_bus(2 downto 0);
		end if;
	end process;

	-- Controle do endereco da ROM
	rom_a <= rom_adhi			when rom_wr_en = '1'               and sltsl_rom = '0'	else	-- Gera endereco para gravacao da FLASH
	         mr_addr			when addr_bus(15 downto 14) = "01" and sltsl_rom = '0'	else	-- Gera endereco do Nextor somente na pagina 1 (4000-7FFF)
	         (others => '-');

	-- Controle do /CE da ROM
	rcewr  <= '1'	when rd_n = '0' or (rom_wr_en = '1' and wr_n = '0')		else '0';

	rom_cs <=  '0' when rcewr = '1' and sltsl_rom = '0' 												else	-- Gravacao da flash
	           '0' when rd_n = '0' and  sltsl_rom = '0' and addr_bus(15 downto 14) = "01"	else	-- Nextor
	           '1';

	-- Controle do /WR da ROM
	rom_we <= '0'  when  rom_wr_en = '1' and wr_n = '0' and sltsl_rom = '0' 					else	-- Ativa sinal de gravacao quando habilitado
	          '1';

end Behavioral;

