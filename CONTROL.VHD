		-- control module (implements MIPS control unit)
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

ENTITY control IS
   PORT( 	
	Opcode 		: IN 	STD_LOGIC_VECTOR( 5 DOWNTO 0 );
	Opcode_out	: OUT STD_LOGIC_VECTOR( 5 DOWNTO 0 );
	RegDst 		: OUT 	STD_LOGIC; -- ID
	ALUSrc 		: OUT 	STD_LOGIC; -- EX
	MemtoReg 	: OUT 	STD_LOGIC; -- WB
	RegWrite 	: OUT 	STD_LOGIC; -- WB
	--For forwarding
	RegWrite_1	: OUT		STD_LOGIC;
	MemRead_Stall :OUT 	STD_LOGIC; 
	MemRead 		: OUT 	STD_LOGIC; -- MEM
	MemWrite 	: OUT 	STD_LOGIC; -- MEM
	Branch 		: OUT 	STD_LOGIC; -- IF
	--Added branch on not equal and Jump 
	Branch_Not_Equal	: OUT		STD_LOGIC; -- IF
	Jump			: OUT		STD_LOGIC; -- IF
	--For stalling
	LW_Stall : IN STD_LOGIC;
	Branch_Stall : IN STD_LOGIC;
	
	ALUop 		: OUT 	STD_LOGIC_VECTOR( 1 DOWNTO 0 ); -- EX
	clock, reset	: IN 	STD_LOGIC );

END control;



ARCHITECTURE behavior OF control IS

	SIGNAL  R_format, Lw, Sw, Beq, Bne, J, RegDst_IDEX, ALUSrc_IDEX 	: STD_LOGIC;
	SIGNAL  MemtoReg_IDEX, MemtoReg_EXMEM, MemtoReg_MEMWB, RegWrite_IDEX, RegWrite_EXMEM, RegWrite_MEMWB : STD_LOGIC;
	SIGNAL  MemRead_IDEX, MemRead_EXMEM, MemWrite_IDEX, MemWrite_EXMEM 	: STD_LOGIC;
	SIGNAL  Branch_IDEX, Branch_Not_Equal_IDEX, Jump_IDEX, Jump_EXMEM, Branch_Stall_1,Branch_Stall_2		: STD_LOGIC;
	SIGNAL  ALUOp_IDEX	: STD_LOGIC_VECTOR( 1 DOWNTO 0);

BEGIN           
	Opcode_out <= Opcode;
				-- Code to generate control signals using opcode bits
	R_format 	<=  '1'  WHEN  Opcode = "000000"  ELSE '0';
	Lw          <=  '1'  WHEN  Opcode = "100011"  ELSE '0';
 	Sw          <=  '1'  WHEN  Opcode = "101011"  ELSE '0';
   Beq         <=  '1'  WHEN  Opcode = "000100"  ELSE '0';
	-- Adding Branch on not equal (Bne) and Jump (J) based on opcode
	Bne			<=  '1'  WHEN  Opcode = "000101"  ELSE '0';
	J				<=  '1'  WHEN  Opcode = "000010"  ELSE '0';
  	RegDst		    	<=  R_format;
 	ALUSrc_IDEX  		<=  Lw OR Sw;
	MemtoReg_IDEX 		<=  Lw;
  	RegWrite_IDEX 		<=  (R_format OR Lw) WHEN LW_Stall = '0' AND Branch_Stall_2 = '0' ELSE '0';
  	MemRead_IDEX 		<=  Lw;
   MemWrite_IDEX 		<=  Sw WHEN LW_Stall = '0' AND Branch_Stall_2 = '0' ELSE '0'; 
 	Branch     			<=  Beq;
	--Control unit out for Branch on not equal and Jump
	Branch_Not_Equal	<=  Bne;
	Jump_IDEX			<=  J;
	ALUOp_IDEX( 1 ) 	<=  R_format;
	ALUOp_IDEX( 0 ) 	<=  Beq OR Bne;
	
	--For forwarding
	RegWrite_1 <= RegWrite_MEMWB;
	--For lw stalling, if MemRead_Stall is high, the instruction is LW
	MemRead_Stall <= MemRead_EXMEM;
	
	
	
PROCESS
	BEGIN
		WAIT UNTIL clock'EVENT AND clock = '1';
		--Pipelining
		
		ALUSrc <= ALUSrc_IDEX;
		MemtoReg_EXMEM <= MemtoReg_IDEX;
		MemtoReg_MEMWB <= MemtoReg_EXMEM;
		MemtoReg	<= MemtoReg_MEMWB;
		RegWrite_EXMEM <= RegWrite_IDEX;
		RegWrite_MEMWB <= RegWrite_EXMEM;
		RegWrite <= RegWrite_MEMWB;
		MemRead_EXMEM <= MemRead_IDEX;
		MemRead <= MemRead_EXMEM;
		MemWrite_EXMEM <= MemWrite_IDEX;
		MemWrite <= MemWrite_EXMEM;
		
		
		
		Branch_Not_Equal<= Branch_Not_Equal_IDEX;
		
		Jump_EXMEM <= Jump_IDEX;
		Jump <= Jump_EXMEM;
		ALUOp <= ALUOp_IDEX;
		--Needed to flush next instruction, not current instruction
		Branch_Stall_1 <= Branch_Stall;
		Branch_Stall_2 <= Branch_Stall_1;
		
		
	END PROCESS;
END behavior;


