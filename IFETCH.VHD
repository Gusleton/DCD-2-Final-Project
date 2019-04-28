-- Ifetch module (provides the PC and instruction 
--memory for the MIPS computer)
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;


LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

LIBRARY lpm;
USE lpm.lpm_components.all;

ENTITY Ifetch IS
	PORT(	SIGNAL Instruction 		: BUFFER	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- ID
        	SIGNAL PC_plus_4_out 	: OUT	STD_LOGIC_VECTOR( 9 DOWNTO 0 ); -- EX
			-- PC_out goes nowhere, just used as out, will need it later for stalls
			SIGNAL PC_out 				: OUT	STD_LOGIC_VECTOR( 9 DOWNTO 0 ); 
        	SIGNAL Add_Result 		: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
			--Added Jump_result value
			SIGNAL Jump_result      : IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
        	SIGNAL Branch 				: IN 	STD_LOGIC;
			--Added BNE and Jump inputs
			SIGNAL Branch_Not_Equal	: IN  STD_LOGIC;
			SIGNAL Jump					: IN 	STD_LOGIC;
        	SIGNAL Zero 				: IN 	STD_LOGIC;
			--Added to check for LW stall conditions
			SIGNAL Branch_Stall		: BUFFER STD_LOGIC;
			SIGNAL LW_Stall			: IN	STD_LOGIC;
			
			SIGNAL Hit_out				: OUT STD_LOGIC;
			SIGNAL State_out			: OUT STD_LOGIC;
			SIGNAL Instruction_Memory_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
			SIGNAL Instruction_Cache_out : OUT STD_LOGIC_VECTOR(35 DOWNTO 0);
			SIGNAL Mem_Addr_out		: OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
			
        	SIGNAL clock, reset 		: IN 	STD_LOGIC);
END Ifetch;

ARCHITECTURE behavior OF Ifetch IS
	SIGNAL Instruction_Previous, Instruction_Memory: STD_LOGIC_VECTOR(31 DOWNTO 0 );
	SIGNAL Instruction_IFID, Instruction_Cache	 : STD_LOGIC_VECTOR(35 DOWNTO 0);
	SIGNAL PC, PC_plus_4_IFID, PC_plus_4_IDEX : STD_LOGIC_VECTOR( 9 DOWNTO 0 );
	SIGNAL next_PC, Mem_Addr : STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL LW_Branch, LW_Branch_1 : STD_LOGIC;
	
	TYPE State_type IS (B,C); -- Define the states
	SIGNAL State : State_Type;	 -- Signal that uses the different states
	SIGNAL Index : STD_LOGIC_VECTOR(2 DOWNTO 0);
	SIGNAL cache_write, Hit : STD_LOGIC;
	SIGNAL write_clock : STD_LOGIC;
	
BEGIN
						--ROM for Instruction Memory, this will feed into the cache
inst_memory: altsyncram
	
	GENERIC MAP (
		operation_mode => "ROM",
		width_a => 32,								--data input width
		widthad_a => 8,							--address input width, 256 blocks
		lpm_type => "altsyncram",
		outdata_reg_a => "UNREGISTERED",
		init_file => "Lab09program.MIF",
		intended_device_family => "Cyclone"
	)
	PORT MAP (
		clock0     => clock,						--clock input
		address_a 	=> Mem_Addr, 				--address input
		q_a 			=> Instruction_Memory);	--data output, goes to cache
		
				
						-- Cache memory unit, implemeted using 

cache: altsyncram
	GENERIC MAP  (
		operation_mode => "SINGLE_PORT",
		width_a => 36,
		widthad_a => 3,
		lpm_type => "altsyncram",
		outdata_reg_a => "UNREGISTERED",
		intended_device_family => "Cyclone"
	)
	PORT MAP (
		wren_a => cache_write,
		clock0 => write_clock,
		address_a => Index,
		data_a => Instruction_Cache, --Input data
		q_a => Instruction_IFID	);	  --Output data
-- Load memory address register with write clock
		write_clock <= NOT clock;
		
		Instruction_Cache <= '1' & Mem_Addr(4 DOWNTO 2) & Instruction_Memory(31 DOWNTO 0); -- To make the data 36 bits wide
		Instruction_Cache_out <= Instruction_Cache;
		Instruction_Memory_out <= Instruction_Memory;
		
						-- Calculates Index for each memory location
	PROCESS(Mem_Addr, Index) IS
		BEGIN
			IF (Mem_Addr = X"00" OR Mem_Addr = X"08" OR Mem_Addr = X"10" OR Mem_Addr = X"18" OR Mem_Addr = X"20") THEN
				Index <= "000";
			ELSIF (Mem_Addr = X"01" OR Mem_Addr = X"09" OR Mem_Addr = X"11" OR Mem_Addr = X"19" OR Mem_Addr = X"21") THEN
				Index <= "001";
			ELSIF (Mem_Addr = X"02" OR Mem_Addr = X"0A" OR Mem_Addr = X"12" OR Mem_Addr = X"1A" OR Mem_Addr = X"22") THEN
				Index <= "010";
			ELSIF (Mem_Addr = X"03" OR Mem_Addr = X"0B" OR Mem_Addr = X"13" OR Mem_Addr = X"1B" OR Mem_Addr = X"23") THEN
				Index <= "011";
			ELSIF (Mem_Addr = X"04" OR Mem_Addr = X"0C" OR Mem_Addr = X"14" OR Mem_Addr = X"1C" OR Mem_Addr = X"24") THEN
				Index <= "100";
			ELSIF (Mem_Addr = X"05" OR Mem_Addr = X"0D" OR Mem_Addr = X"15" OR Mem_Addr = X"1D" OR Mem_Addr = X"25") THEN
				Index <= "101";
			ELSIF (Mem_Addr = X"06" OR Mem_Addr = X"0E" OR Mem_Addr = X"16" OR Mem_Addr = X"1E" OR Mem_Addr = X"26") THEN
				Index <= "110";
			ELSE --(Mem_Addr = X"07" OR Mem_Addr = X"0F" OR Mem_Addr = X"17" OR Mem_Addr = X"1F" OR Mem_Addr = X"27") THEN
				Index <= "111";
			END IF;
	END PROCESS;
		
	Hit_out <= Hit;		
	Mem_Addr_out <= Mem_Addr;
					-- Cache state machine	
	PROCESS (clock, reset) IS
	  BEGIN 
		 IF (reset = '1') THEN
			State <= B;
	 
		 ELSIF ( clock'EVENT ) AND ( clock = '1' ) THEN
		 
				CASE State IS
					
					WHEN B =>
							State_out <= '0';
							cache_write <= '0';
					-- if tag in cache = tag for requested address and valid = true
						IF ( Instruction_IFID(34 DOWNTO 32) = Mem_Addr(7 DOWNTO 5) AND Instruction_IFID(35) = '1') THEN
							Hit <=  '1';
							
							-- Last edit 
							IF (LW_Stall = '1' OR LW_Branch = '1' OR LW_Branch_1 = '1') THEN
								Instruction <= X"00000000";
							ELSE 
								Instruction <= Instruction_IFID(31 DOWNTO 0);
							END IF;
							
						ELSE 
							Hit <= '0';
							Instruction <= X"00000000";
							State <= C;
						END IF;
					
		
					WHEN C =>
							State_out <= '1';
							-- Progess to B since memory is constantly spitting stuff out anyways
							Hit <= '0'; -- Continue to stall
							cache_write <= '1';
							State <= B;
							--Keep here for safety
							Instruction <= X"00000000";
							
							
					WHEN others =>
							State <= B;
				END CASE;
		END IF;
	END PROCESS;
												
		
		

		
--		PROCESS(LW_Stall, Instruction, Instruction_IFID, LW_Branch) IS
--			BEGIN
--				IF LW_Stall = '1' OR LW_Branch = '1' OR LW_Branch_1 = '1' OR Hit = '0' THEN
--					Instruction <= X"00000000";
--				ELSE 
--					Instruction <= Instruction_IFID(31 DOWNTO 0);
--				END IF;
--		END PROCESS;

					-- Instructions always start on word address - not byte
		PC(1 DOWNTO 0) <= "00";
		
					-- copy output signals - allows read inside module
		PC_out 			<= PC;
		
		
						-- send address to inst. memory address register
		Mem_Addr <= Next_PC;
		
						-- Adder to increment PC by 4        
      PC_plus_4_IFID( 9 DOWNTO 2 )  <= PC( 9 DOWNTO 2 ) + 1;
      PC_plus_4_IFID( 1 DOWNTO 0 )  <= "00";
			
						-- If branch is taken, stall and flush
		Branch_Stall <= '1' WHEN  (( ( Branch = '1' ) AND ( Zero = '1' ) ) OR ((Branch_Not_Equal	 = '1') AND (Zero = '0')))
		ELSE '0';
		
						-- Detect LW Branch hazard
		LW_Branch <= '1' WHEN Instruction_Previous(31 DOWNTO 26) = "100011" AND (Instruction_IFID(31 DOWNTO 26) = "000100" OR Instruction_IFID(31 DOWNTO 26) = "000101")
		ELSE '0';
						-- Mux to select Branch Address or PC + 4 or PC      
		Next_PC  <= X"00" WHEN Reset = '1' 
		--Added BNE = '1' AND Zero = '0' to allow branch on not equal
			ELSE Add_Result WHEN Branch_Stall = '1' 
		--Added Jump 
			ELSE Jump_result WHEN (Jump = '1')
			ELSE PC( 9 DOWNTO 2 ) WHEN (LW_Stall = '1' AND LW_Branch = '0') OR LW_Branch_1 = '1' OR LW_Branch = '1' OR Hit = '0'
			
			ELSE PC_plus_4_IFID( 9 DOWNTO 2 );

	PROCESS
		BEGIN
			WAIT UNTIL ( clock'EVENT ) AND ( clock = '1' );
			IF reset = '1' THEN
				   PC( 9 DOWNTO 2) <= "00000000" ; 
			ELSE 
				   PC( 9 DOWNTO 2 ) <= next_PC;
			END IF;
			-- Added for pipelining
			PC_plus_4_out <= PC_plus_4_IFID;
			
			Instruction_Previous <= Instruction_IFID(31 DOWNTO 0);
			LW_Branch_1 <= LW_Branch;
		
			
	END PROCESS;
END behavior;


