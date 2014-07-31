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
--Remember to add this library when you use xilinx primitives from Language templates.
library unisim;
use unisim.vcomponents.all;

entity sd is
	port(
		clock				: in    std_logic;
		reset_n			: in    std_logic;
		dis_mapper		: in    std_logic;								-- Informacao se a mapper esta ativa
		mr_mp				: in    std_logic;								-- 1 = mapper, 0 = megaram

		-- CPU interface
		cs					: in    std_logic;								-- Selecao (chip select) da porta SPI
		addr_bus			: in    std_logic;								-- porta 0 ou 1 (0 = comando, 1 = data)
		data_bus			: inout std_logic_vector(7 downto 0);
		wr_n				: in    std_logic;
		rd_n				: in    std_logic;

		-- SD card interface
		sd_cs				: out   std_logic_vector(1 downto 0);		-- Saida Chip Select para os cartoes
		sd_sclk			: out   std_logic;								-- Saida SCK
		sd_mosi			: out   std_logic;								-- Master Out Slave In
		sd_miso			: in    std_logic;								-- Master In Slave Out
		sd_writeprot	: in    std_logic_vector(1 downto 0);		-- 0 = cartao protegido contra escrita
		sd_inserted		: in    std_logic_vector(1 downto 0)		-- 0 = cartao inserido
	);

end entity;

architecture Behavioral of sd is

	-- Component Declaration for FDCP should be placed 
	-- after architecture statement but before begin keyword 
	component FDCP
		-- synthesis translate_off
		generic (INIT : bit := '0');
		-- synthesis translate_on
		port (
			Q : out STD_ULOGIC;
			C : in STD_ULOGIC;
			CLR : in STD_ULOGIC;
			D : in STD_ULOGIC;
			PRE : in STD_ULOGIC
		);
	end component;

	signal cs_spi			: std_logic;
	signal sd_cs1			: std_logic;
	signal sd_cs0			: std_logic;
	signal busy				: std_logic;
	signal port0_wr		: std_logic;
	signal port1_cs		: std_logic;
	signal port0_reg		: std_logic_vector(7 downto 0);
	signal port1_reg		: std_logic_vector(7 downto 0);

	-- State type of the SPI transfer state machine
	type   state_type is (s_idle, s_running, s_done);
	signal state           : state_type;
	-- Shift register
	signal shift_reg       : std_logic_vector(7 downto 0);
	-- Buffer to hold data to be sent
	signal spi_data_buf    : std_logic_vector(7 downto 0);
	-- Start transmission flag
	signal start           : std_logic;
	-- Number of bits transfered
	signal count           : unsigned(3 downto 0);
	-- Buffered SPI clock
	signal spi_clk_buf     : std_logic;
	-- Buffered SPI clock output
	signal spi_clk_out     : std_logic;
	-- Previous SPI clock state
	signal prev_spi_clk    : std_logic;

	signal ff_q, ff_clr : std_logic;

begin

	flipflop1: FDCP
	-- synthesis translate_off
	generic map (INIT => '0')
	-- synthesis translate_on
	port map (
		Q => ff_q,
		C => clock,
		CLR => ff_clr,
		D => start,
		PRE => '0'
	);

	-- Enable portas
	cs_spi	<= '1'	when cs = '1' and (wr_n = '0' or rd_n = '0')				else '0';	-- SPI selecionada e executando leitura ou gravacao
	port0_wr	<= '1'	when cs_spi = '1' and wr_n = '0' and addr_bus = '0'	else '0';	-- Write porta 0 (comando)
	port1_cs <= '1'	when cs_spi = '1' and addr_bus = '1'						else '0';	-- Write ou Read porta 1

	-- Leitura das portas
	data_bus <= 
		port0_reg	when cs_spi = '1' and addr_bus = '0' and rd_n = '0'  else					-- Leitura porta 0 (comando/status)
		port1_reg	when cs_spi = '1' and addr_bus = '1' and rd_n = '0'  else					-- Leitura porta 1 (data)
		(others => 'Z');

	-- Escrita porta 0
	process(port0_wr, reset_n)
	begin
		if reset_n = '0' then
			sd_cs1 <= '1';
			sd_cs0 <= '1';
		elsif rising_edge(port0_wr) then
			sd_cs1 <= data_bus(1);
			sd_cs0 <= data_bus(0);
		end if;
	end process;

	-- Acesso porta 1
	process (reset_n, port1_cs, ff_clr)
	begin
		if (reset_n = '0' or ff_clr = '1') then
			spi_data_buf <= (others => '1');
			start <= '0';
		elsif rising_edge(port1_cs) then

			if rd_n = '0' then
				spi_data_buf <= (others => '1');		-- Uma leitura seta 0xFF para enviar e dispara a transmissao
			else
				spi_data_buf <= data_bus;				-- Uma escrita seta o valor a enviar e dispara a transmissao
			end if;
			start <= '1';
		
		end if;
	end process;

	--------------------------------------------------
	-- Essa parte lida com a porta SPI por hardware --
	--      Implementa um SPI Master Mode 0         --
	--------------------------------------------------
	port0_reg	<= busy & "0" & mr_mp & dis_mapper & sd_writeprot & sd_inserted;		-- Monta byte de status
	sd_cs			<= sd_cs1 & sd_cs0;																	-- Joga CSs para chips externos

	busy	<= start;

	-- SPI write
	process(clock, reset_n)
	begin		
		if reset_n = '0' then
			ff_clr <= '0';
		elsif rising_edge(clock) then
		
			prev_spi_clk <= spi_clk_buf;
			case state is

				when s_idle =>
					if ff_q = '1' then
						count     <= (others => '0');
						shift_reg <= spi_data_buf;
						state     <= s_running;
					end if;

				when s_running =>
					if prev_spi_clk = '1' and spi_clk_buf = '0' then
						spi_clk_out <= '0';
						count       <= count + 1;
						shift_reg   <= shift_reg(6 downto 0) & sd_miso;
						if count = "0111" then
							state <= s_done;
							--start	<= '0';
							ff_clr <= '1';
						end if;
					elsif prev_spi_clk = '0' and spi_clk_buf = '1' then
						spi_clk_out <= '1';
					end if;

				when s_done =>
					port1_reg	<= shift_reg;
					state			<= s_idle;
					ff_clr <= '0';
				when others =>
					null;
			end case;
		end if;
	end process;

	-- Generate SPI clock
	spi_clock_gen : process(clock, reset_n)
	begin
		if reset_n = '0' then
			spi_clk_buf   <= '0';
		elsif rising_edge(clock) then
			if state = s_running then
				spi_clk_buf <= not spi_clk_buf;
			else
				spi_clk_buf <= '0';
			end if;
		end if;
	end process;

	sd_mosi <= shift_reg(7);
	sd_sclk <= spi_clk_out;

end Behavioral;
