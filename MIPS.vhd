				-- Top Level Structural Model for MIPS Processor Core
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;

ENTITY MIPS IS

	PORT( reset, clock																	: IN 	STD_LOGIC; 
		-- Output important signals to pins for easy display in Simulator
		PC,PC_plus_4_out																					: OUT STD_LOGIC_VECTOR( 9 DOWNTO 0 );
		ALU_result_out, read_data_1_out, read_data_2_out, write_data_out,	
     	Instruction_out, memory_write_data_out, Ainput_out, Binput_out		: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 );
		--Added BNE_out and Jump_out
		Branch_out, Zero_Detected, Memwrite_out, BNE_out, Jump_out, 
		Regwrite_out, selectWBA_out, selectWBB_out, LW_Stall_out,MemRead_Stall_out, RegDst_out, Hit_out		: OUT STD_LOGIC ;
		IF_ID_RegisterRs_out, IF_ID_RegisterRt_out, write_address_out		: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
		ForwardA_ctl_out, ForwardB_ctl_out, Branch_Check_A_Out,Branch_Check_B_Out	: OUT	STD_LOGIC_VECTOR( 1 DOWNTO 0 );
		Add_result_out, Branch_Add, Sign_Extend_IDEX : OUT STD_LOGIC_VECTOR( 7 DOWNTO 0);
		State_out : OUT STD_LOGIC;
		Instruction_Memory_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		Instruction_Cache_out : OUT STD_LOGIC_VECTOR(35 DOWNTO 0);
		Mem_Addr_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
		
END 	MIPS;

ARCHITECTURE structure OF MIPS IS

	COMPONENT Ifetch
   	     PORT(	SIGNAL Instruction 		: BUFFER	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- ID
						SIGNAL PC_plus_4_out 	: OUT	STD_LOGIC_VECTOR( 9 DOWNTO 0 ); -- EX
						-- PC_out goes nowhere, just used as out, will need it later for stalls
						SIGNAL PC_out 				: OUT	STD_LOGIC_VECTOR( 9 DOWNTO 0 ); 
						SIGNAL Add_Result 		: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
						--Added Jump_result value
						SIGNAL Jump_result      : IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
						SIGNAL Branch 				: IN 	STD_LOGIC;
						SIGNAL Branch_Stall		: BUFFER  STD_LOGIC;
						--Added BNE and Jump inputs
						SIGNAL Branch_Not_Equal	: IN  STD_LOGIC;
						SIGNAL Jump					: IN 	STD_LOGIC;
						SIGNAL Zero 				: IN 	STD_LOGIC;
						--Added to check for LW stall conditions
						SIGNAL LW_Stall			: IN	STD_LOGIC;
						
						SIGNAL Hit_out				: OUT STD_LOGIC;
						SIGNAL State_out			: OUT STD_LOGIC;
						SIGNAL Instruction_Memory_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
						SIGNAL Instruction_Cache_out : OUT STD_LOGIC_VECTOR(35 DOWNTO 0);
						SIGNAL Mem_Addr_out		: OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
						SIGNAL clock, reset 		: IN 	STD_LOGIC);
	END COMPONENT; 

	COMPONENT Idecode
 	     PORT(	read_data_1	: BUFFER 	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- EX
			read_data_2	: BUFFER 	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- EX
			Sign_extend : OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 ); -- EX
			--Added Jump_Offset which will come straight from the jump instruction and goes straight into an adder
			Jump_Offset : OUT 	STD_LOGIC_VECTOR( 9 DOWNTO 0 ); -- EX
			--Added for forwarding
			ALU_Result       : IN STD_LOGIC_VECTOR( 31 DOWNTO 0 );
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
			
			LW_Stall_Two_Ago	: OUT	STD_LOGIC;
			LW_Stall			: BUFFER STD_LOGIC;
			register_address_LW_EXMEM : OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
			--For Branch detection
			PC_plus_4		: IN 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
			Add_Result 		: BUFFER	STD_LOGIC_VECTOR( 7 DOWNTO 0 ); -- IF
			Zero				: BUFFER STD_LOGIC;
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
	END COMPONENT;

	COMPONENT control
	     PORT( 	Opcode 		: IN 	STD_LOGIC_VECTOR( 5 DOWNTO 0 );
					Opcode_out	: OUT STD_LOGIC_VECTOR( 5 DOWNTO 0 );
					RegDst 		: OUT 	STD_LOGIC; -- ID
					ALUSrc 		: OUT 	STD_LOGIC; -- EX
					MemtoReg 	: OUT 	STD_LOGIC; -- WB
					RegWrite 	: OUT 	STD_LOGIC; -- WB
					--For forwarding
					RegWrite_1	: OUT		STD_LOGIC;
					MemRead_Stall :OUT STD_LOGIC; 
					MemRead 		: OUT 	STD_LOGIC; -- MEM
					MemWrite 	: OUT 	STD_LOGIC; -- MEM
					Branch 		: OUT 	STD_LOGIC; -- IF
					
					--Added branch on not equal and Jump 
					Branch_Not_Equal	: OUT		STD_LOGIC; -- IF
					Jump			: OUT		STD_LOGIC; -- IF
					--For stalling
					LW_Stall			: IN STD_LOGIC;
					Branch_Stall   : IN STD_LOGIC;
					ALUop 			: OUT 	STD_LOGIC_VECTOR( 1 DOWNTO 0 ); -- EX
					clock, reset	: IN 	STD_LOGIC );
	END COMPONENT;

	COMPONENT  Execute
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
			LW_Stall				: IN STD_LOGIC;
			LW_Stall_Two_Ago	: IN STD_LOGIC;
			register_address_LW_EXMEM : IN STD_LOGIC_VECTOR( 4 DOWNTO 0);
			--For testing purposes
			ALU_ctl_out		: OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
			ALUOp_out		: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			ForwardA_ctl_out	: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			ForwardB_ctl_out	: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0 );
			Ainput_out			: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			Binput_out			: OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 );
			
			clock, reset	: IN 	STD_LOGIC );
	END COMPONENT;


	COMPONENT dmemory
	     PORT(	read_data 			: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
        		address 					: IN 		STD_LOGIC_VECTOR( 7 DOWNTO 0 );
        		write_data 				: IN 		STD_LOGIC_VECTOR( 31 DOWNTO 0 );
        		MemRead, Memwrite 	: IN 		STD_LOGIC;
        		Clock,reset				: IN 		STD_LOGIC );
	END COMPONENT;

					-- declare signals used to connect VHDL components
	SIGNAL PC_plus_4 		: STD_LOGIC_VECTOR( 9 DOWNTO 0 );
	SIGNAL read_data_1 	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL read_data_2 	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL Sign_Extend 	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL Add_Result 	: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL ALU_result_WB 	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL ALU_result_MEM 	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL read_data 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL ALUSrc 			: STD_LOGIC;
	SIGNAL Branch 			: STD_LOGIC;
	SIGNAL Branch_Stall 	: STD_LOGIC;
	
	--Added signals BNE, Jump, Jump_result, Jump_Offset 
	SIGNAL Branch_Not_Equal	: STD_LOGIC;
	SIGNAL Jump				: STD_LOGIC;
	SIGNAL Jump_result	: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL Jump_Offset	: STD_LOGIC_VECTOR( 9 DOWNTO 0 );
	SIGNAL RegDst 			: STD_LOGIC;
	SIGNAL RegWrite 		: STD_LOGIC;
	SIGNAL Zero 			: STD_LOGIC;
	SIGNAL MemWrite 		: STD_LOGIC;
	SIGNAL MemtoReg 		: STD_LOGIC;
	SIGNAL MemRead 		: STD_LOGIC;
	SIGNAL ALUop 			: STD_LOGIC_VECTOR(  1 DOWNTO 0 );
	SIGNAL Instruction	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	
	--Added for forwarding
	SIGNAL RegWrite_1		   : STD_LOGIC;
	SIGNAL ALU_Result       : STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL IF_ID_RegisterRs : STD_LOGIC_VECTOR( 4 DOWNTO 0); 
	SIGNAL IF_ID_RegisterRt : STD_LOGIC_VECTOR( 4 DOWNTO 0); 
	SIGNAL write_data_forwarding : STD_LOGIC_VECTOR( 31 DOWNTO 0 ); 
	SIGNAL write_register_address_forwarding_1 : STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	SIGNAL write_register_address_forwarding_2 : STD_LOGIC_VECTOR( 4 DOWNTO 0 );
	--Added for stalling
	SIGNAL LW_Branch_Stall  : STD_LOGIC;
	SIGNAL LW_Stall_Two_Ago	: STD_LOGIC;
	SIGNAL LW_Stall			: STD_LOGIC;
	SIGNAL MemRead_Stall		: STD_LOGIC;
	SIGNAL register_address_LW_EXMEM : STD_LOGIC_VECTOR( 4 DOWNTO 0);
	SIGNAL Zero_out			: STD_LOGIC;
	SIGNAL Branch_Add_out	: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL Sign_extend_IDEX_out : STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL write_address		: STD_LOGIC_VECTOR( 4 DOWNTO 0);
	SIGNAL Branch_Check_A	: STD_LOGIC_VECTOR( 1 DOWNTO 0 );
	SIGNAL Branch_Check_B	: STD_LOGIC_VECTOR( 1 DOWNTO 0 );
	
BEGIN
					-- copy important signals to output pins for easy 
					-- display in Simulator
   Instruction_out 	<= Instruction;
   ALU_result_out 	<= ALU_result_MEM;
   read_data_1_out 	<= read_data_1;
   read_data_2_out 	<= read_data_2;
   write_data_out  	<= read_data WHEN MemtoReg = '1' ELSE ALU_result_WB;
   Branch_out 		<= Branch;
	BNE_out			<= Branch_Not_Equal;
	Jump_out			<= Jump;
	Zero_Detected  <= Zero_out;
   RegWrite_out 	<= RegWrite;
   MemWrite_out 	<= MemWrite;	
	memory_write_data_out <= read_data_2 WHEN MemWrite = '1';
	RegWrite_1		<= RegWrite_1;
	ALU_Result     <= ALU_Result;
	IF_ID_RegisterRs_out <= IF_ID_RegisterRs;
	IF_ID_RegisterRt_out <= IF_ID_RegisterRt;
	write_data_forwarding <= write_data_forwarding;
	write_register_address_forwarding_1 <= write_register_address_forwarding_1; 
	write_register_address_forwarding_2 <= write_register_address_forwarding_2;
	LW_Stall_out <= LW_Stall;
	MemRead_Stall_out <= MemRead_Stall;
	Add_result_out <= Add_result;
	Branch_Add <= Branch_Add_out;
	Sign_extend_IDEX <= Sign_extend_IDEX_out;
	PC_plus_4_out <= PC_plus_4;
	write_address_out <= write_address;
	Branch_Check_A_out <= Branch_Check_A;
	Branch_Check_B_out <= Branch_Check_B;
	RegDst_out <= RegDst;
	
					-- connect the 5 MIPS components   
  IFE : Ifetch
	PORT MAP (	Instruction 	=> Instruction,
    	    	PC_plus_4_out 	=> PC_plus_4,
				Add_Result 		=> Add_Result,
				Jump_result    => Jump_result,
				Branch 			=> Branch,
				Branch_Not_Equal => Branch_Not_Equal,
				Jump				=> Jump,
				Zero 				=> Zero,
				Branch_Stall   => Branch_Stall,
				PC_out 			=> PC,
        		LW_Stall			=> LW_Stall,
				Hit_out			=> Hit_out,
				State_out		=> State_out,
				Instruction_Memory_out => Instruction_Memory_out,
				Instruction_Cache_out => Instruction_Cache_out,
				Mem_Addr_out => Mem_Addr_out,
				clock 			=> clock,  
				reset 			=> reset );

   ID : Idecode
   	PORT MAP (	read_data_1 	=> read_data_1,
        		read_data_2 	=> read_data_2,
        		Instruction 	=> Instruction,
				Zero				=> Zero,
        		read_data 		=> read_data,
				PC_plus_4		=> PC_plus_4,
				ALU_Result     => ALU_Result,
				
				IF_ID_RegisterRs => IF_ID_RegisterRs,
				IF_ID_RegisterRt => IF_ID_RegisterRt,
				write_data_forwarding => write_data_forwarding,
				write_register_address_forwarding_1 => write_register_address_forwarding_1, 
				write_register_address_forwarding_2 => write_register_address_forwarding_2,
				ALU_result_WB 		=> ALU_result_WB,
				RegWrite 		=> RegWrite,
				MemtoReg 		=> MemtoReg,
				RegDst 			=> RegDst,
				Add_Result 		=> Add_Result,
				Sign_extend 	=> Sign_extend,
				Jump_Offset    => Jump_Offset,
				selectWBA_out  => selectWBA_out,
				selectWBB_out  => selectWBB_out,
				MemRead_Stall => MemRead_Stall,
				LW_Stall_Two_Ago => LW_Stall_Two_Ago,
				LW_Stall => LW_Stall,
				register_address_LW_EXMEM => register_address_LW_EXMEM,
				Zero_out	=> Zero_out,
				Branch_Add_out => Branch_Add_out,
				Sign_extend_IDEX_out => Sign_extend_IDEX_out,
				write_address => write_address,
				
        		clock 			=> clock,  
				reset 			=> reset );


   CTL:   control
	PORT MAP ( 	Opcode 			=> Instruction( 31 DOWNTO 26 ),
				RegDst 			=> RegDst,
				ALUSrc 			=> ALUSrc,
				MemtoReg 		=> MemtoReg,
				RegWrite 		=> RegWrite,
				RegWrite_1		=> RegWrite_1,
				MemRead 			=> MemRead,
				MemRead_Stall => MemRead_Stall,
				MemWrite 		=> MemWrite,
				Branch 			=> Branch,
				LW_Stall 		=> LW_Stall,
				--Added signals BNE and Jump
				Branch_Not_Equal => Branch_Not_Equal,
				Branch_Stall   => Branch_Stall,
				Jump				=> Jump,
				ALUop 			=> ALUop,
            clock 			=> clock,
				reset 			=> reset );

   EXE:  Execute
   	PORT MAP (	Read_data_1 	=> read_data_1,
            Read_data_2 	=> read_data_2,
				Sign_extend 	=> Sign_extend,
            --Function_opcode	=> Instruction( 5 DOWNTO 0 ), replace with sign_extend
				ALUOp 			=> ALUop,
				Jump_Offset		=> Jump_Offset,
				ALUSrc 			=> ALUSrc,
				
            ALU_Result_MEM		=> ALU_Result_MEM,
				ALU_Result_WB		=> ALU_Result_WB,
				
				Jump_Result    => Jump_Result,
				PC_plus_4		=> PC_plus_4,
				-- Used for forwarding
				ALU_Result => ALU_Result,
				RegWrite => RegWrite,
				RegWrite_1 => RegWrite_1,
				IF_ID_RegisterRs => IF_ID_RegisterRs,
				IF_ID_RegisterRt => IF_ID_RegisterRt,
				write_data_forwarding => write_data_forwarding,
				write_register_address_forwarding_1 => write_register_address_forwarding_1, 
				write_register_address_forwarding_2 => write_register_address_forwarding_2,
				LW_Stall_Two_Ago => LW_Stall_Two_Ago,
				LW_Stall => LW_Stall,
				register_address_LW_EXMEM => register_address_LW_EXMEM,
				
				-- For testing
				ForwardA_ctl_out => ForwardA_ctl_out,
				ForwardB_ctl_out => ForwardB_ctl_out,
				Ainput_out => Ainput_out,
				Binput_out => Binput_out,
				
            Clock				=> clock,
				Reset				=> reset );

   MEM:  dmemory
	PORT MAP (	read_data 		=> read_data,
				address 			=> ALU_Result_MEM (7 DOWNTO 0),
				write_data 		=> read_data_2,
				MemRead 			=> MemRead, 
				Memwrite 		=> MemWrite, 
            clock 			=> clock,  
				reset 			=> reset );
END structure;

