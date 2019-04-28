						--  Dmemory module (implements the data
						--  memory for the MIPS computer)
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

ENTITY dmemory IS
	PORT(	read_data 				: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- WB
        	address 					: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
        	write_data 				: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	   	MemRead, Memwrite 	: IN 	STD_LOGIC;
         clock,reset				: IN 	STD_LOGIC );
END dmemory;

ARCHITECTURE behavior OF dmemory IS
SIGNAL write_clock : STD_LOGIC;
SIGNAL read_data_MEMWB : STD_LOGIC_VECTOR( 31 DOWNTO 0);
BEGIN
	data_memory : altsyncram
	GENERIC MAP  (
		operation_mode => "SINGLE_PORT",
		width_a => 32,
		widthad_a => 8,
		lpm_type => "altsyncram",
		outdata_reg_a => "UNREGISTERED",
		init_file => "Lab06memory.mif",
		intended_device_family => "Cyclone"
	)
	PORT MAP (
		wren_a => memwrite,
		clock0 => write_clock,
		address_a => address,
		data_a => write_data,
		q_a => read_data_MEMWB	);
-- Load memory address register with write clock
		write_clock <= NOT clock;
		
PROCESS
		BEGIN
			WAIT UNTIL ( clock'EVENT ) AND ( clock = '1' );
			-- Added for pipelining
			read_data <= read_data_MEMWB;
	END PROCESS;
END behavior;

