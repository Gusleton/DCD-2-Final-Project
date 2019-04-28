						--  Idecode module (implements the register file for
LIBRARY IEEE; 			-- the MIPS computer)
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY Idecode IS
	  PORT(	read_data_1	: BUFFER 	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- EX
			read_data_2	: BUFFER 	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- EX
			Sign_extend : OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- EX
			--Added Jump_Offset which will come straight from the jump instruction and goes straight into an adder
			Jump_Offset : OUT 	STD_LOGIC_VECTOR( 9 DOWNTO 0 ); -- EX
			--Added for forwarding
			ALU_Result       : IN STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			--ALU_Result_MEM : OUT	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			IF_ID_RegisterRs : BUFFER STD_LOGIC_VECTOR( 4 DOWNTO 0); -- EX
			IF_ID_RegisterRt : BUFFER STD_LOGIC_VECTOR( 4 DOWNTO 0); -- EX
			write_data_forwarding : OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- not pipelined
			write_register_address_forwarding_1 : OUT STD_LOGIC_VECTOR( 4 DOWNTO 0 ); --not pipelined
			write_register_address_forwarding_2 : OUT STD_LOGIC_VECTOR( 4 DOWNTO 0 ); --not pipelined
			--Altered Instruction for pipelining purposes
			Instruction : IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			read_data 	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			ALU_result_WB	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			RegWrite 	: IN 	STD_LOGIC;
			MemtoReg 	: IN 	STD_LOGIC;
			RegDst 		: IN 	STD_LOGIC;
			--For LW stalling
			MemRead_Stall  : IN STD_LOGIC;
			
			LW_Stall_Two_Ago	: BUFFER	STD_LOGIC;
			LW_Stall			: BUFFER STD_LOGIC;
			register_address_LW_EXMEM : OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
			--For Branch detection
			PC_plus_4		: IN 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
			Add_Result 		: BUFFER	STD_LOGIC_VECTOR( 7 DOWNTO 0 ); -- IF
			Zero 				: BUFFER	STD_LOGIC; -- IF
			--FoR TESTING PURPOSES
			selectWBA_out : OUT STD_LOGIC;
			selectWBB_out : OUT STD_LOGIC;
			Zero_out		  : OUT STD_LOGIC;
			Branch_Add_out: OUT STD_LOGIC_VECTOR( 7 DOWNTO 0 );
			Sign_extend_IDEX_out : OUT STD_LOGIC_VECTOR( 7 DOWNTO 0 );
			write_address	: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
			Branch_Check_A	: BUFFER STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			Branch_Check_B	: BUFFER STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			
			
			
			clock,reset	: IN 	STD_LOGIC );
END Idecode;


ARCHITECTURE behavior OF Idecode IS
TYPE register_file IS ARRAY ( 0 TO 31 ) OF STD_LOGIC_VECTOR( 31 DOWNTO 0 );

	SIGNAL register_array					: register_file;
	SIGNAL write_register_address 		: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_register_address_IDEX  : STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_register_address_EXMEM : STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_register_address_MEMWB : STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_data							: STD_LOGIC_VECTOR( 31 DOWNTO 0 );	
	SIGNAL read_register_1_address		: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL read_register_2_address		: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_register_address_1		: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_register_address_0		: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL Instruction_immediate_value	: STD_LOGIC_VECTOR( 15 DOWNTO 0 );
	SIGNAL read_data_1_IDEX					: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL read_data_2_IDEX					: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL Sign_extend_IDEX					: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL Jump_Offset_IDEX 				: STD_LOGIC_VECTOR( 9 DOWNTO 0 );
	-- added for forwarding
	SIGNAL selectWBA							: STD_LOGIC;
	SIGNAL selectWBB							: STD_LOGIC;
	SIGNAL LW_Stall_Previous				: STD_LOGIC;
	SIGNAL register_address_LW_IDEX		: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL register_address_LW				: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL Branch_Add 						: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL Add_Result_IDEX 					: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL read_data_1_temp					: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL read_data_2_temp					: STD_LOGIC_VECTOR( 31 DOWNTO 0 ); 
	
	SIGNAL ALU_Result_MEMWB					: STD_LOGIC_VECTOR( 31 DOWNTO 0 ); 
	
	
	
	
BEGIN
	read_register_1_address 	<= Instruction( 25 DOWNTO 21 );
   read_register_2_address 	<= Instruction( 20 DOWNTO 16 );
   write_register_address_1	<= Instruction( 15 DOWNTO 11 );
   write_register_address_0 	<= Instruction( 20 DOWNTO 16 );
   Instruction_immediate_value <= Instruction( 15 DOWNTO 0 );
	
					-- Zero detector
	Zero <= '1' WHEN (read_data_1_IDEX = read_data_2_IDEX)
	ELSE '0';
	
	Zero_out <= Zero;
	
	Branch_Add_out <= Branch_Add;
	Sign_extend_IDEX_out <= Sign_extend_IDEX(7 DOWNTO 0);
	
					-- Adder to compute Branch Address
	Branch_Add	<= PC_plus_4( 9 DOWNTO 2 ) + Instruction( 7 DOWNTO 0 ) ;
	Add_Result_IDEX 	<= Branch_Add( 7 DOWNTO 0 );
	
					-- Read Register 1 Operation including forwarding answer from 3 instructions ago
	read_data_1_temp <= register_array( CONV_INTEGER( read_register_1_address ) )
		WHEN selectWBA = '0' ELSE write_data;
					
					-- Read Register 2 Operation including forwarding answer from 3 instructions ago
	read_data_2_temp <= register_array( CONV_INTEGER( read_register_2_address ) )
		WHEN selectWBB = '0' ELSE write_data;
		
					-- Mux for Register Write Address
   write_register_address_IDEX <= write_register_address_1 
			WHEN RegDst = '1'  ELSE write_register_address_0;
			
					-- Mux to bypass data memory for Rformat instructions
	write_data <= ALU_result_WB( 31 DOWNTO 0 ) 
			WHEN ( MemtoReg = '0' ) 	ELSE read_data;
			
					-- Sign Extend 16-bits to 32-bits
   Sign_extend_IDEX <= X"0000" & Instruction_immediate_value
		WHEN Instruction_immediate_value(15) = '0'
		ELSE	X"FFFF" & Instruction_immediate_value;
		
					-- Jump_Offset for jump calculation
	Jump_Offset_IDEX <= Instruction( 7 DOWNTO 0) & "00";
	
	--FOR TESTING PURPOSES
	selectWBA_out <= selectWBA;
	selectWBB_out <= selectWBB;
	
	--for forwarding purposes
	write_data_forwarding <= write_data;
	write_register_address_forwarding_2 <= write_register_address;	-- newer instruction
	write_register_address_forwarding_1 <= write_register_address_MEMWB; -- older instruction
	register_address_LW <= Instruction( 20 DOWNTO 16 ); --Rt write address in LW
	
	--This forwarding logic unit checks for reading from a register that is being written to
	-- due to a previous intstruction. selectWBA and selectWBB are selects for two muxes
PROCESS ( RegWrite, write_register_address, read_register_1_address, read_register_2_address, Instruction) IS
	BEGIN
		IF (RegWrite = '1' AND (write_register_address = read_register_1_address) AND read_register_1_address /= "0000" 
		AND Instruction(31 DOWNTO 26) /= "000100" AND Instruction(31 DOWNTO 26) /= "000101" AND Instruction(31 DOWNTO 26) /= "100011") THEN
			selectWBA <= '1';
		ELSE
			selectWBA <= '0';
		END IF;
		IF (RegWrite = '1' AND (write_register_address = read_register_2_address) AND read_register_2_address /= "0000" 
		AND Instruction(31 DOWNTO 26) /= "000100" AND Instruction(31 DOWNTO 26) /= "000101" AND Instruction(31 DOWNTO 26) /= "100011") THEN
			selectWBB <= '1';
		ELSE
			selectWBB <= '0';
		END IF;
END PROCESS;

	-- This stuff checks for load word-use Hazards and stalls the pipeline if needed
PROCESS ( MemRead_Stall, write_register_address_0, read_register_1_address, read_register_2_address, Instruction, LW_Stall_Previous) IS
	BEGIN
		-- If the current instruction is LW and it is writing to an adddress used by the next instruction then stall
		IF (MemRead_Stall = '1' AND (write_register_address_0 = read_register_1_address OR write_register_address_0 = read_register_2_address)) THEN
			LW_Stall <= '1';
		
		ELSE 
			LW_Stall <= '0';
			
			
		END IF;
END PROCESS;



		-- Forwarding unit for the Branch-Zero detector
		-- If branch needs something from ALU
		---- forward that
		-- If branch doesn't need something from ALU but from ALU result in the mem stage
		---- foward that
		-- If branch needs something thats currently being written
		---- do nothing because there are already muxes behind these that take care of that
PROCESS ( read_register_1_address, read_register_2_address, write_register_address_EXMEM, write_register_address_MEMWB) IS
	BEGIN
		IF (read_register_1_address = write_register_address_EXMEM AND write_register_address_EXMEM /= "0000") THEN
			Branch_Check_A <= "01";
		ELSIF (read_register_1_address = write_register_address_MEMWB AND write_register_address_MEMWB /= "0000") THEN
			Branch_Check_A <= "10";
		ELSIF ( RegWrite = '1' AND RegWrite = '0') THEN -- filler condition
			Branch_Check_A <= "11";
		ELSE 
			Branch_Check_A <= "00";
		END IF;
		IF (read_register_2_address = write_register_address_EXMEM AND write_register_address_EXMEM /= "0000") THEN
			Branch_Check_B <= "01";
		ELSIF (read_register_2_address = write_register_address_MEMWB AND write_register_address_MEMWB /= "0000") THEN
			Branch_Check_B <= "10";
		ELSIF ( RegWrite = '1' AND RegWrite = '0') THEN-- filler condition
			Branch_Check_B <= "11";
		ELSE 
			Branch_Check_B <= "00";
		END IF;
END PROCESS;

		-- Muxes to select what to send to Branch detector
PROCESS( Branch_Check_A, Branch_Check_B) IS
	BEGIN
		CASE Branch_Check_A IS
			WHEN "00" => read_data_1_IDEX <= read_data_1_temp; --source is either register or write data
			WHEN "01" => read_data_1_IDEX <= ALU_Result;
			WHEN "10" => read_data_1_IDEX <= ALU_Result_MEMWB;
			WHEN "11" => read_data_1_IDEX <= read_data_1_temp; --never happens
		END CASE;
		CASE Branch_Check_B IS
			WHEN "00" => read_data_2_IDEX <= read_data_2_temp; --source is either register or write data
			WHEN "01" => read_data_2_IDEX <= ALU_Result;
			WHEN "10" => read_data_2_IDEX <= ALU_Result_MEMWB;
			WHEN "11" => read_data_2_IDEX <= read_data_2_temp; --never happens
		END CASE;
END PROCESS;





Add_Result <= Add_Result_IDEX;
write_address <= write_register_address;
PROCESS
	BEGIN
		WAIT UNTIL clock'EVENT AND clock = '1';
		IF reset = '1' THEN
					-- Initial register values on reset are register = reg#
					-- use loop to automatically generate reset logic 
					-- for all registers
			FOR i IN 0 TO 31 LOOP
				register_array(i) <= CONV_STD_LOGIC_VECTOR( i, 32 );
 			END LOOP;
					-- Write back to register - don't write to register 0
  		ELSIF RegWrite = '1' AND write_register_address /= 0 THEN
		      register_array( CONV_INTEGER( write_register_address)) <= write_data;
		END IF;
		--Pipelining
		read_data_1 <= read_data_1_IDEX;
		read_data_2 <= read_data_2_IDEX;
		Sign_extend <= Sign_extend_IDEX;
		Jump_Offset <= Jump_Offset_IDEX;
		write_register_address_EXMEM <= write_register_address_IDEX;
		write_register_address_MEMWB <= write_register_address_EXMEM;
		write_register_address <= write_register_address_MEMWB;
		--Used for forwarding
		IF_ID_RegisterRs <= read_register_1_address;
		IF_ID_RegisterRt <= read_register_2_address;
		
		LW_Stall_Previous <= LW_Stall;
		LW_Stall_Two_Ago <= LW_Stall_Previous;
		
		--Used for LW used forwarding in execute stage, needs to be held for two cycles to line up with correct instruction
		register_address_LW_IDEX <= register_address_LW;
		register_address_LW_EXMEM <= register_address_LW_IDEX;
		
		ALU_Result_MEMWB <= ALU_Result;
		
		
	END PROCESS;
END behavior;


