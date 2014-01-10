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

-- Implementa uma mapper padrao de 512K para SRAM

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity mapper is
	port(
		reset_n		: in    std_logic;								-- /RESET
		cpu_a			: in    std_logic_vector(15 downto 0);		-- Barramento de enderecos da CPU
		cpu_d			: inout std_logic_vector(7 downto 0);		-- Barramento de dados da CPU
		ioFC_FF		: in    std_logic;								-- Sinal de selecao de I/O
		cpu_rd_n		: in    std_logic;								-- /RD da CPU
		cpu_wr_n		: in    std_logic;								-- /WR da CPU
		sltsl_n		: in    std_logic;								-- /SLTSL do slot/subslot da RAM
		sram_ma		: out   std_logic_vector(4 downto 0);		-- Saida do banco da SRAM
		sram_cs_n	: out   std_logic									-- Saida de selecao da SRAM
	);
end mapper;

architecture rtl of mapper is

	signal port_cs     : std_logic;
	signal MapBank0    : std_logic_vector(4 downto 0);
	signal MapBank1    : std_logic_vector(4 downto 0);
	signal MapBank2    : std_logic_vector(4 downto 0);
	signal MapBank3    : std_logic_vector(4 downto 0);
	signal VS          : std_logic_vector(4 downto 0);

begin

  ----------------------------------------------------------------
  -- Mapper bank register access
  ----------------------------------------------------------------

	port_cs <= '1' when ioFC_FF = '1' and cpu_wr_n = '0'		else '0';

	process(reset_n, port_cs)
	begin
		if (reset_n = '0') then
			MapBank0   <= "00011";		-- Reset configura blocos padroes da mapper
			MapBank1   <= "00010";
			MapBank2   <= "00001";
			MapBank3   <= "00000";
		elsif (rising_edge(port_cs)) then
			case cpu_a(1 downto 0) is
				when "00"   => MapBank0 <= cpu_d(4 downto 0);
				when "01"   => MapBank1 <= cpu_d(4 downto 0);
				when "10"   => MapBank2 <= cpu_d(4 downto 0);
				when others => MapBank3 <= cpu_d(4 downto 0);
			end case;

		end if;
	end process;

	-- Leitura dos registros da mapper pelas portas
	cpu_d <= (OTHERS => 'Z') when cpu_rd_n = '1' or ioFC_FF = '0' else
	      "000" & MapBank0 when cpu_a(1 downto 0) = "00" else
	      "000" & MapBank1 when cpu_a(1 downto 0) = "01" else
	      "000" & MapBank2 when cpu_a(1 downto 0) = "10" else
	      "000" & MapBank3;

	-- Gera endereco da SRAM de acordo com endereco do barramento e bancos configurados
	VS <= MapBank0 when cpu_a(15 downto 14) = "00" else
	      MapBank1 when cpu_a(15 downto 14) = "01" else
	      MapBank2 when cpu_a(15 downto 14) = "10" else
	      MapBank3;

	-- Pega parte baixa do endereco que vai direto nos pinos da SRAM
	sram_ma <= VS(4 downto 0);

	sram_cs_n <= sltsl_n;

end rtl;
