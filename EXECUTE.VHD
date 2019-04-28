--  Execute module (implements the data ALU and Branch Address Adder  
--  for the MIPS computer)
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

ENTITY  Execute IS
	PORT(	
			ALU_Result_MEM : OUT	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- MEM
			ALU_result_WB	: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- WB
			
			-- Adding Jump_Result to allow for Jump command
			Jump_result		: OUT STD_LOGIC_VECTOR( 7 DOWNTO 0 ); -- IF
			Read_data_1 	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			Read_data_2 	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			Sign_extend 	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			ALUOp 			: IN 	STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			--Added Jump_Offset input to calculate jump value
			Jump_Offset 	: IN 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
			ALUSrc 			: IN 	STD_LOGIC;
			PC_plus_4		: IN 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
			-- Used for forwarding
			ALU_Result       : OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			RegWrite			  : IN STD_LOGIC;
			RegWrite_1		  : IN STD_LOGIC;
			IF_ID_RegisterRs : IN STD_LOGIC_VECTOR( 4 DOWNTO 0); 
			IF_ID_RegisterRt : IN STD_LOGIC_VECTOR( 4 DOWNTO 0); 
			write_data_forwarding : IN STD_LOGIC_VECTOR( 31 DOWNTO 0 ); 
			write_register_address_forwarding_1 : IN STD_LOGIC_VECTOR( 4 DOWNTO 0 );
			write_register_address_forwarding_2 : IN STD_LOGIC_VECTOR( 4 DOWNTO 0 );
			LW_Stall_Two_Ago	: IN STD_LOGIC;
			LW_Stall				: IN STD_LOGIC;
			register_address_LW_EXMEM : IN STD_LOGIC_VECTOR( 4 DOWNTO 0);
			
			--For testing purposes
			ALU_ctl_out		: OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
			ALUOp_out		: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			ForwardA_ctl_out	: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			ForwardB_ctl_out	: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			Ainput_out			: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			Binput_out			: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			
			clock, reset	: IN 	STD_LOGIC );
END Execute;

ARCHITECTURE behavior OF Execute IS

SIGNAL Ainput, Binput 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
SIGNAL ALU_output_mux		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );

--Added Jump_Add for processing of jump address
SIGNAL Jump_Add 				: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
SIGNAL ALU_ctl					: STD_LOGIC_VECTOR( 2 DOWNTO 0 );

SIGNAL ALU_Result_EXMEM 	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
SIGNAL ALU_Result_MEMWB		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );

SIGNAL Jump_result_EXMEM	: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
SIGNAL Function_opcode		: STD_LOGIC_VECTOR( 5 DOWNTO 0 );
--Used in forwarding
SIGNAL ForwardA_out			: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
SIGNAL ForwardB_out			: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
SIGNAL ForwardC_out			: STD_LOGIC_VECTOR( 4 DOWNTO 0 );
SIGNAL ForwardA_ctl			: STD_LOGIC_VECTOR( 1 DOWNTO 0 );
SIGNAL ForwardB_ctl			: STD_LOGIC_VECTOR( 1 DOWNTO 0 );


BEGIN
	Ainput <= ForwardA_out;
						
	Binput <= ForwardB_out 
		WHEN ( ALUSrc = '0' ) 
  		ELSE  Sign_extend( 31 DOWNTO 0 );
		
		
						-- Define function opcode source--
	Function_opcode <= Sign_extend(5 DOWNTO 0);
	
						-- Generate ALU control bits
	ALU_ctl( 0 ) <= ( Function_opcode( 0 ) OR Function_opcode( 3 ) ) AND ALUOp(1 );	--add or subu? and r-type
	ALU_ctl( 1 ) <= ( NOT Function_opcode( 2 ) ) OR (NOT ALUOp( 1 ) );					-- not sub or not r-type
	ALU_ctl( 2 ) <= ( Function_opcode( 1 ) AND ALUOp( 1 )) OR ALUOp( 0 );				-- r-type or branch and addu
	
    
		
						-- Select ALU output        
	ALU_result_EXMEM <= X"0000000" & B"000"  & ALU_output_mux( 31 ) 
		WHEN  ALU_ctl = "111" 
		ELSE  ALU_output_mux( 31 DOWNTO 0 );
	
						-- NEW CODE Adder to compute Jump Address
	Jump_Add		<= PC_plus_4( 9 DOWNTO 2 ) +  Jump_Offset( 9 DOWNTO 2 ) ;
	Jump_result_EXMEM <= Jump_Add( 7 DOWNTO 0 );
	
	-- FOR TESTING
	ALUOp_out <= ALUOp;
	ALU_ctl_out <= ALU_ctl;
	ForwardA_ctl_out <= ForwardA_ctl;
	ForwardB_ctl_out <= ForwardB_ctl;
	Ainput_out <= Ainput;
	Binput_out <= Binput;
	
	
	-- Regsiter File forwarding hazards
	ALU_Result <= ALU_result_EXMEM;
	
	--FORWARDING UNIT, control signals for ALU input muxes
	-- If forwardA_ctl = 00 the first ALU operand comes from the register file
	-- If forwardA_ctl = 10 the first ALU operand comes from the previous ALU result
	-- If forwardA_ctl = 01 the first ALU operand comes from data memory or an earlier ALU result
	-- Same rules apply respectively to forwardB_ctl
PROCESS( RegWrite_1, write_register_address_forwarding_1, IF_ID_RegisterRs, write_register_address_forwarding_2, 
			register_address_LW_EXMEM, LW_Stall_Two_Ago, LW_Stall) IS
	BEGIN
		IF (RegWrite_1 = '1' AND write_register_address_forwarding_1 /= "0000" AND write_register_address_forwarding_1 = IF_ID_RegisterRs) THEN 
			ForwardA_ctl <= "10";
		ELSIF (RegWrite_1	 = '1' AND write_register_address_forwarding_2 /= "0000" 
		AND write_register_address_forwarding_1 /= IF_ID_RegisterRs AND write_register_address_forwarding_2 = IF_ID_RegisterRs) THEN
			ForwardA_ctl <= "01";
		ELSIF (LW_Stall_Two_Ago = '1' AND register_address_LW_EXMEM = IF_ID_RegisterRs AND LW_Stall = '0') THEN
			ForwardA_ctl <= "11"; -- LW Use case
		ELSE	
			ForwardA_ctl <= "00"; --normal operation, no forwarding
		END IF;
END PROCESS;
		
PROCESS( RegWrite_1, write_register_address_forwarding_1, IF_ID_RegisterRt, write_register_address_forwarding_2, 
			register_address_LW_EXMEM,LW_Stall_Two_Ago, LW_Stall) IS
	BEGIN
		IF (RegWrite_1 = '1' AND write_register_address_forwarding_1 /= "0000" AND write_register_address_forwarding_1 = IF_ID_RegisterRt) THEN 
			ForwardB_ctl <= "10";
		ELSIF (RegWrite_1 = '1' AND write_register_address_forwarding_2 /= "0000" 
		AND write_register_address_forwarding_1 /= IF_ID_RegisterRt AND write_register_address_forwarding_2 = IF_ID_RegisterRt) THEN
			ForwardB_ctl <= "01";
		ELSIF (LW_Stall_Two_Ago = '1' AND register_address_LW_EXMEM = IF_ID_RegisterRt AND LW_Stall = '0') THEN
			ForwardB_ctl <= "11"; -- LW Use case
		ELSE
			ForwardB_ctl <= "00"; --normal operation, no forwarding
		END IF;
END PROCESS;

	--ForwardA MUX
	PROCESS ( ForwardA_ctl)
		BEGIN
			CASE ForwardA_ctl IS
				WHEN "00" => ForwardA_out <= read_data_1; --source is registers
				WHEN "01" => ForwardA_out <= write_data_forwarding; -- source is MEMWB
				WHEN "10" => ForwardA_out <= ALU_result_MEMWB; --source is ALU result
				WHEN "11" => ForwardA_out <= write_data_forwarding;  -- Used for LW use stalls
			END CASE;
	END PROCESS;
	--ForwardB MUX
	PROCESS ( ForwardB_ctl)
		BEGIN
			CASE ForwardB_ctl IS
				WHEN "00" => ForwardB_out <= read_data_2; --source is registers
				WHEN "01" => ForwardB_out <= write_data_forwarding; -- source is MEMWB
				WHEN "10" => ForwardB_out <= ALU_result_MEMWB; --source is ALU result
				WHEN "11" => ForwardB_out <= write_data_forwarding; -- Used for LW use stalls
			END CASE;
	END PROCESS;


PROCESS ( ALU_ctl, Ainput, Binput )
	BEGIN
					-- Select ALU operation
 	CASE ALU_ctl IS
						-- ALU performs ALUresult = A_input AND B_input
		WHEN "000" 	=>	ALU_output_mux 	<= Ainput AND Binput; 
						-- ALU performs ALUresult = A_input OR B_input
     	WHEN "001" 	=>	ALU_output_mux 	<= Ainput OR Binput;
						-- ALU performs ALUresult = A_input + B_input
	 	WHEN "010" 	=>	ALU_output_mux 	<= Ainput + Binput;
						-- ALU performs ?
 	 	WHEN "011" 	=>	ALU_output_mux 	<= X"00000000";
						-- ALU performs ?
 	 	WHEN "100" 	=>	ALU_output_mux 	<= X"00000000";
						-- ALU performs ?
 	 	WHEN "101" 	=>	ALU_output_mux 	<= X"00000000";
						-- ALU performs ALUresult = A_input - B_input
 	 	WHEN "110" 	=>	ALU_output_mux 	<= Ainput - Binput;
						-- ALU performs SLT
  	 	WHEN "111" 	=>	ALU_output_mux 	<= Ainput - Binput ;
 	 	WHEN OTHERS	=>	ALU_output_mux 	<= X"00000000" ;
  	END CASE;
  END PROCESS;
PROCESS
	BEGIN
		WAIT UNTIL clock'EVENT AND clock = '1';
				--Pipelining
				ALU_Result_MEM <= ALU_Result_EXMEM;
				
				ALU_Result_MEMWB <= ALU_Result_EXMEM;
				ALU_Result_WB <= ALU_Result_MEMWB;
				
				
				Jump_result <= Jump_result_EXMEM;

 END PROCESS;
END behavior;

