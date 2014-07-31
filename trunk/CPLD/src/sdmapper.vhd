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
	generic (
		dividirclock	: boolean := FALSE
	);
	port(
		clock				: in    std_logic;
		reset_n			: in    std_logic;
		dis_mapper		: in    std_logic;
		mr_mp				: in    std_logic;								-- 1 = mapper

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
		ram_a				: out   std_logic_vector(18 downto 13);
		ram_cs			: out   std_logic;
		ram_we			: out   std_logic;

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
	signal fw_en			: std_logic;
	signal iomapper		: std_logic;
	signal iomegaram		: std_logic;
	signal ffff				: std_logic;
	signal sltsl_c			: std_logic;
	signal slt_exp_n		: std_logic_vector(3 downto 0);
	signal sltsl_rom		: std_logic;
	signal sltsl_ram		: std_logic;

	-- Porta SPI
	signal sd_en			: std_logic;		-- '0' acessa ROM, '1' acessa porta SPI (4000-42FF e 4800)
	signal sd_addr			: std_logic;
	signal sd_pcs			: std_logic;
	signal sd_chav			: std_logic;
	signal clock_sd		: std_logic;

	-- flash
	signal rom_chav		: std_logic;
	signal mr_addr			: std_logic_vector(2 downto 0);
	signal rom_rd1			: std_logic;
	signal rom_rd2			: std_logic;
	signal rom_rd			: std_logic;
	signal rom_wr			: std_logic;
	signal rom_wr_en		: std_logic;
	signal rom_adhi		: std_logic_vector(2 downto 0);

begin

	-- Porta SPI
	portaspi: entity work.sd
	port map (
		clock				=> clock_sd,
		reset_n			=> reset_n,
		dis_mapper		=> dis_mapper,
		mr_mp				=> mr_mp,
		-- CPU interface
		cs					=> sd_pcs,
		addr_bus			=> sd_addr,
		data_bus			=> data_bus,
		wr_n				=> wr_n,
		rd_n				=> rd_n,
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
	mpmr: entity work.megamapper
	port map (
		reset_n		=> reset_n,
		cpu_a			=> addr_bus,
		cpu_d			=> data_bus,
		mr_mp			=> mr_mp,
		ioFx			=> iomapper,
		io8x			=> iomegaram,
		cpu_rd_n		=> rd_n,
		cpu_wr_n		=> wr_n,
		sltsl_n		=> sltsl_ram,
		sram_ma		=> ram_a,
		sram_cs_n	=> ram_cs,
		sram_we_n	=> ram_we
	);

	-- Glue Logic

	-- FFFF eh 1 quando todos os bits do barramento de enderecos for 1 (usado no expansor de slots)
	ffff    <= AND_REDUCE(addr_bus);

	-- Slot Selects
	sltsl_c		<= sltsl_n      when dis_mapper = '1' else '1';
	sltsl_rom	<= slt_exp_n(1) when dis_mapper = '1' else sltsl_n;
	sltsl_ram	<= slt_exp_n(3) when dis_mapper = '1' else '1';

	-- Enable portas I/O
	io_cs			<= not iorq_n and m1_n;

	fw_en			<= '1' when io_cs = '1' and addr_bus(7 downto 0) = X"5F" and wr_n = '0'	else '0';	-- Acesso I/O escrita porta $5F
	iomapper		<= '1' when io_cs = '1' and addr_bus(7 downto 2) = "111111"					else '0';	-- Acesso I/O portas $FC a $FF
	iomegaram	<= '1' when io_cs = '1' and addr_bus(7 downto 1) = "1000111"				else '0';	-- Acesso I/O portas $8E a $8F

	-- Escrita porta $B7
	process (reset_n, fw_en)
	begin
		if reset_n = '0' then
			rom_wr_en <= '0';
			rom_adhi  <= (others => '0');
		elsif rising_edge(fw_en) then
			rom_wr_en	<= data_bus(7);
			rom_adhi		<= data_bus(2 downto 0);
		end if;
	end process;

	-- Controle da MEGAROM ASCII16 do Nextor
	-- Gravacao no endereco $6000 chaveia o banco somente se nao estiver no modo gravacao da flash
	rom_chav <= '1'	when wr_n = '0' and sltsl_rom = '0' and addr_bus = X"6000" and rom_wr_en = '0'	else '0';
	-- Gravacao no endereco $6001 chaveia entre ROM e SPI
	sd_chav	<= '1'	when wr_n = '0' and sltsl_rom = '0' and addr_bus = X"6001" and rom_wr_en = '0'	else '0';

	process (reset_n, rom_chav)
	begin
		if reset_n = '0' then 
			mr_addr <= (OTHERS => '0');
		elsif rising_edge(rom_chav) then
			mr_addr <= data_bus(2 downto 0);
		end if;
	end process;

	-- ROM acessada para leitura na pagina 1 ou pagina 2 ou gravacao em qualquer pagina
	rom_rd1	<= '1'	when sltsl_rom = '0' and rd_n = '0' and addr_bus(15 downto 14) = "01"	else '0';
	rom_rd2	<= '1'	when sltsl_rom = '0' and rd_n = '0' and addr_bus(15 downto 14) = "10"	else '0';
	rom_wr	<= '1'	when sltsl_rom = '0' and wr_n = '0' and rom_wr_en = '1'						else '0';
	rom_rd	<= rom_rd1 or rom_rd2;

	-- Controle do endereco da ROM
	rom_a <= rom_adhi			when rom_wr_en = '1'               and sltsl_rom = '0'	else	-- Gera endereco para gravacao da FLASH
	         mr_addr			when addr_bus(15 downto 14) = "01" and sltsl_rom = '0'	else	-- Gera endereco do Nextor somente na pagina 1 (4000-7FFF)
	         (others => '-');

	-- Controle do /CE da ROM
	rom_cs <= '0' when rom_wr = '1' and sd_en = '0'		else	-- Gravacao da Flash (se SPI estiver desligado)
	          '0' when rom_rd = '1' and sd_en = '0'		else	-- Leitura da Flash (se SPI estiver desligado)
	          '0' when rom_rd = '1' and sd_pcs = '0'	else	-- Leitura da Flash (se SPI estiver ligado e nao for enderecado)
	          '1';

	-- Controle do /WR da ROM
	rom_we <= '0' when rom_wr = '1' and sd_en = '0' 	else	-- Ativa sinal de gravacao quando habilitado e SPI desativado
	          '1';

	-- Controle do cheveamento da porta SPI
	process (reset_n, sd_chav)
	begin
		if reset_n = '0' then 
			sd_en		<= '0';
		elsif rising_edge(sd_chav) then
			sd_en		<= data_bus(0);
		end if;
	end process;

	sd_pcs	<= '1'  when sd_en = '1' and sltsl_rom = '0' and rom_wr_en = '0' and
	                      (addr_bus(15 downto 11) = "01000" or addr_bus = X"4800")  else
	            '0';
	sd_addr	<= not addr_bus(11);	-- diferencia $4000-47FF de $4800

	divclk: if dividirclock generate
		process (reset_n, clock)
		begin
			if reset_n = '0' then
				clock_sd <= '0';
			elsif rising_edge(clock) then
				clock_sd <= not clock_sd;
			end if;
		end process;
	end generate;
	ndivclk: if not dividirclock generate
		clock_sd	<= clock;
	end generate;

end Behavioral;
