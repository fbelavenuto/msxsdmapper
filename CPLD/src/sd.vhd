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

-- Implementa uma porta SPI padrao por I/O

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sd is
	port(
		clock				: in    std_logic;
		reset_n			: in    std_logic;
		dis_mapper		: in    std_logic;							-- Informacao se a mapper esta ativa

		-- CPU interface
		addr_bus			: in    std_logic_vector(7 downto 0);
		data_bus			: inout std_logic_vector(7 downto 0);
		wr_n				: in    std_logic;
		rd_n				: in    std_logic;
		iorq_n			: in    std_logic;
		m1_n				: in    std_logic;

		-- SD card interface
		sd_cs				: out   std_logic_vector(1 downto 0);	-- Saida Chip Select para os cartoes
		sd_sclk			: out   std_logic;							-- Saida SCK
		sd_mosi			: out   std_logic;							-- Master Out Slave In
		sd_miso			: in    std_logic;							-- Master In Slave Out
		sd_writeprot	: in    std_logic_vector(1 downto 0);	-- 0 = cartao protegido contra escrita
		sd_inserted		: in    std_logic_vector(1 downto 0)	-- 0 = cartao inserido
	);

end entity;

architecture Behavioral of sd is

	signal io_cs	  		: std_logic;
	signal sd_en    		: std_logic;
	signal sck_delayed	: std_logic;
	signal counter			: unsigned(3 downto 0);
	signal sd_cs1			: std_logic;
	signal sd_cs0			: std_logic;
	signal busy				: std_logic;
	-- Shift register has an extra bit because we write on the
	-- falling edge and read on the rising edge
	signal shift_reg		: std_logic_vector(8 downto 0);
	signal port08_reg		: std_logic_vector(7 downto 0);
	signal port09_reg		: std_logic_vector(7 downto 0);

begin

	-- Enable portas I/O
	io_cs	<= not iorq_n and m1_n;
	sd_en	<= '1'  when io_cs = '1' and addr_bus(7 downto 1) = "0000100"	else '0';	-- Acesso I/O portas 8 e 9

	-- Leitura das portas
	data_bus <= 
		port08_reg  when sd_en = '1' and addr_bus = X"08" and rd_n = '0'  else		-- Leitura porta 08
		port09_reg	when sd_en = '1' and addr_bus = X"09" and rd_n = '0'  else		-- Leitura porta 09
		(others => 'Z');

	--------------------------------------------------
	-- Essa parte lida com a porta SPI por hardware --
	--      Implementa um SPI Master Mode 0         --
	--------------------------------------------------
	busy			<= not (counter(3) and counter(2) and counter(1) and counter(0));	-- 1 está ocupado no meio da transmissao
	port08_reg	<= busy & "00" & dis_mapper & sd_writeprot & sd_inserted;			-- Monta byte de status
	sd_cs			<= sd_cs1 & sd_cs0;																-- Joga CSs para chips externos

	process(clock, reset_n)
	begin
		if reset_n = '0' then
			sd_cs1 <= '1';
			sd_cs0 <= '1';
		elsif rising_edge(clock) then
			if sd_en = '1' and wr_n = '0' and addr_bus = X"08" then				-- Escrita porta 08
				sd_cs1 <= data_bus(1);
				sd_cs0 <= data_bus(0);
			end if;
		end if;
	end process;

	-- SD card outputs from clock divider and shift register
	sd_sclk  <= sck_delayed;
	sd_mosi  <= shift_reg(8);

	-- Atrasa SCK para dar tempo do bit mais significativo mudar de estado e acertar MOSI antes do SCK
	process (clock, reset_n)
	begin
		if reset_n = '0' then
			sck_delayed <= '0';
		elsif rising_edge(clock) then
			sck_delayed <= not counter(0);
		end if;
	end process;

	-- SPI write
	process(clock, reset_n)
	begin		
		if reset_n = '0' then
			shift_reg  <= (others => '1');
			port09_reg <= (others => '1');
			counter    <= "1111"; -- Idle
		elsif rising_edge(clock) then
			if counter = "1111" then
				-- Store previous shift register value in input register
				port09_reg		<= shift_reg(7 downto 0);
				shift_reg(8)	<= '1';								-- MOSI repousa em '1'

				-- Idle - check for a bus access
				if sd_en = '1' and addr_bus = X"09" then								-- Escrita ou leitura na porta 09
					-- Write loads shift register with data
					-- Read loads it with all 1s
					if rd_n = '0' then
						shift_reg <= (others => '1');										-- Uma leitura seta 0xFF para enviar e dispara a transmissão
					else
						shift_reg <= data_bus & '1';										-- Uma escrita seta o valor a enviar e dispara a transmissão
					end if;
					counter <= "0000"; -- Initiates transfer
				end if;
			else
				counter <= counter + 1;												-- Transfer in progress

				if sck_delayed = '0' then
					shift_reg(0) <= sd_miso;										-- Input next bit on rising edge
				else
					shift_reg <= shift_reg(7 downto 0) & '1';					-- Output next bit on falling edge
				end if;
			end if;
		end if;
	end process;

end Behavioral;
