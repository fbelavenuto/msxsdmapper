--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   05:15:54 04/27/2014
-- Design Name:   
-- Module Name:   D:/Dropbox/My Dropbox/8bits/MSX/SDMapper3/CPLD/src/sd_tb.vhd
-- Project Name:  sdmapper
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: sd
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY sd_tb IS
END sd_tb;
 
ARCHITECTURE behavior OF sd_tb IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT sd
    PORT(
         clock : IN  std_logic;
         reset_n : IN  std_logic;
         dis_mapper : IN  std_logic;
         mr_mp : IN  std_logic;
         cs : IN  std_logic;
         addr_bus : IN  std_logic;
         data_bus : INOUT  std_logic_vector(7 downto 0);
         wr_n : IN  std_logic;
         rd_n : IN  std_logic;
         sd_cs : OUT  std_logic_vector(1 downto 0);
         sd_sclk : OUT  std_logic;
         sd_mosi : OUT  std_logic;
         sd_miso : IN  std_logic;
         sd_writeprot : IN  std_logic_vector(1 downto 0);
         sd_inserted : IN  std_logic_vector(1 downto 0)
        );
    END COMPONENT;
    

   --Inputs
	signal clock_msx : std_logic;
   signal clock : std_logic;
   signal reset_n : std_logic;
   signal dis_mapper : std_logic;
   signal mr_mp : std_logic;
   signal cs : std_logic;
   signal addr_bus : std_logic;
   signal wr_n : std_logic;
   signal rd_n : std_logic;
   signal sd_miso : std_logic;
   signal sd_writeprot : std_logic_vector(1 downto 0);
   signal sd_inserted : std_logic_vector(1 downto 0);

	--BiDirs
   signal data_bus : std_logic_vector(7 downto 0);

 	--Outputs
   signal sd_cs : std_logic_vector(1 downto 0);
   signal sd_sclk : std_logic;
   signal sd_mosi : std_logic;

   -- Clock period definitions
	constant clockmsx_period : time := 280 ns;	-- 3.57 MHz
--	constant clock_period : time := 40 ns;			-- 25 MHz
	constant clock_period : time := 33 ns;			-- 30 MHz
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: sd PORT MAP (
          clock => clock,
          reset_n => reset_n,
          dis_mapper => dis_mapper,
          mr_mp => mr_mp,
          cs => cs,
          addr_bus => addr_bus,
          data_bus => data_bus,
          wr_n => wr_n,
          rd_n => rd_n,
          sd_cs => sd_cs,
          sd_sclk => sd_sclk,
          sd_mosi => sd_mosi,
          sd_miso => sd_miso,
          sd_writeprot => sd_writeprot,
          sd_inserted => sd_inserted
        );

   -- Clock MSX process definitions
   clockmsx_process :process
   begin
		clock_msx <= '1';
		wait for clockmsx_period/2;
		clock_msx <= '0';
		wait for clockmsx_period/2;
   end process;

   -- Clock process definitions
   clock_process :process
   begin
		clock <= '1';
		wait for clock_period/2;
		clock <= '0';
		wait for clock_period/2;
   end process;

	sd_miso <= not sd_mosi;
 
   -- Stimulus process
   stim_proc: process
   begin
		dis_mapper		<= '0';
		mr_mp				<= '0';
		sd_writeprot	<= "00";
		sd_inserted		<= "00";
		reset_n			<= '0';
		cs					<= '0';
		wr_n				<= '1';
		rd_n				<= '1';
		addr_bus 		<= '0';
		data_bus 		<= (others => 'Z');

      -- hold reset state for 100 ns.
      wait for 100 ns;	
		reset_n <= '1';

      wait for 100 ns;

      -- insert stimulus here 
		addr_bus	<= '1';
		data_bus <= X"41";
		wait for 10 ns;
		cs 	<= '1';
		wr_n	<= '0';
		wait for 140 ns;
		cs 	<= '0';
		wr_n	<= '1';
		wait for 1000 ns;
		data_bus <= X"AA";
		wait for 140 ns;
		cs 	<= '1';
		wr_n	<= '0';
		wait for 140 ns;
		data_bus <= "ZZZZZZZZ";
		cs 	<= '0';
		wr_n	<= '1';

      wait;
   end process;

END;
