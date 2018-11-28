
use work.bv_arithmetic.all;
use work.dlx_types.all;

entity aubie_controller is
	generic(
	        prop_delay    : Time := 5 ns;
	        xt_prop_delay : Time := 15 ns -- Extended prop_delay for allowing other signals to propagate first
	);
	port(ir_control: in dlx_word;
	     alu_out: in dlx_word;
	     alu_error: in error_code;
	     clock: in bit;
	     regfilein_mux: out threeway_muxcode;
	     memaddr_mux: out threeway_muxcode;
	     addr_mux: out bit;
	     pc_mux: out threeway_muxcode;
	     alu_func: out alu_operation_code;
	     regfile_index: out register_index;
	     regfile_readnotwrite: out bit;
	     regfile_clk: out bit;
	     mem_clk: out bit;
	     mem_readnotwrite: out bit;
	     ir_clk: out bit;
	     imm_clk: out bit;
	     addr_clk: out bit;
             pc_clk: out bit;
	     op1_clk: out bit;
	     op2_clk: out bit;
	     result_clk: out bit
	     );
end aubie_controller;

architecture behavior of aubie_controller is
begin
	behav: process(clock) is
		type state_type is range 1 to 20;
		variable state: state_type := 1;
		variable opcode: byte;
		variable destination,operand1,operand2 : register_index;

	begin
		if clock'event and clock = '1' then
		   opcode := ir_control(31 downto 24);
		   destination := ir_control(23 downto 19);
		   operand1 := ir_control(18 downto 14);
		   operand2 := ir_control(13 downto 9);
		   case state is
			when 1 => -- fetch the instruction, for all types
				memaddr_mux <= "00" after prop_delay; -- memory threeway_mux input_0 to read from PC
				regfile_clk <= '0' after prop_delay;
				mem_clk <= '1' after prop_delay;
				mem_readnotwrite <= '1' after prop_delay;
				ir_clk <= '1' after prop_delay; -- High so Instruction Register can receive a signal from Memory
				imm_clk <= '0' after prop_delay;
				addr_clk <= '0' after prop_delay;
				pc_clk <= '0' after prop_delay; -- Low so program counter will output the current address it retains
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;


				state := 2;
			when 2 =>

				-- figure out which instruction
			 	if opcode(7 downto 4) = "0000" then -- ALU op
					state := 3;
				elsif opcode = X"20" then  -- STO
					state := 9;
				elsif opcode = X"30" or opcode = X"31" then -- LD or LDI
					state := 7;
				elsif opcode = X"22" then -- STOR
					state := 14;
				elsif opcode = X"32" then -- LDR
					state := 12;
				elsif opcode = X"40" or opcode = X"41" then -- JMP or JZ
					state := 16;
				elsif opcode = X"10" then -- NOOP
					state := 19;
				else -- error
				end if;
			when 3 =>
				-- ALU op:  load op1 register from the regfile
				regfile_index <= operand1 after prop_delay;
				regfile_readnotwrite <= '1' after prop_delay;
				regfile_clk <= '1' after prop_delay;
				mem_clk <= '0' after prop_delay;
				ir_clk <= '0' after prop_delay;
				imm_clk <= '0' after prop_delay;
				addr_clk <= '0' after prop_delay;
				op1_clk <= '1' after prop_delay; -- needs to be high so it can accept regfile data
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;
				state := 4;
			when 4 =>
				-- ALU op: load op2 registear from the regfile
				regfile_index <= operand2 after prop_delay;
				regfile_readnotwrite <= '1' after prop_delay;
				regfile_clk <= '1' after prop_delay;
				mem_clk <= '0' after prop_delay;
				ir_clk <= '0' after prop_delay;
				imm_clk <= '0' after prop_delay;
				addr_clk <= '0' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '1' after prop_delay; -- needs to be high so it can accept regfile data
				result_clk <= '0' after prop_delay;
				state := 5;
			when 5 =>
				-- ALU op:  perform ALU operation
				-- your code here
				alu_func <= opcode(3 downto 0) after prop_delay;
				regfile_clk <= '1' after prop_delay;
				mem_clk <= '0' after prop_delay;
				ir_clk <= '0' after prop_delay;
				imm_clk <= '0' after prop_delay;
				addr_clk <= '0' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay; -- needs to be high so it can accept regfile data
				result_clk <= '1' after prop_delay;
				state := 6;
			when 6 =>
				-- ALU op: write back ALU operation
				-- your code here
				regfilein_mux <= "00" after prop_delay;
				pc_mux <= "00" after prop_delay;
				regfile_index <= destination after prop_delay;
				regfile_readnotwrite <= '0' after prop_delay;
				regfile_clk <= '1' after prop_delay;
				imm_clk <= '0' after prop_delay;
				pc_clk <= '1' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;
				state := 1;
			when 7 =>
				-- LD or LDI: get the addr or immediate word
			  -- your code here
				pc_clk <= '1' after prop_delay;
				pc_mux <= "00" after prop_delay;
				memaddr_mux <= "00" after prop_delay;
				regfile_clk <= '0' after prop_delay;
				mem_readnotwrite <= '1' after prop_delay;
				ir_clk <= '0' after prop_delay;
				imm_clk <= '1' after prop_delay;
				addr_clk <= '0' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;
				state := 8;
			when 8 =>
				-- LD or LDI
				-- your code here
				if (opcode = x"30") then -- LD
					regfilein_mux <= "01" after prop_delay;
					memaddr_mux <= "01" after prop_delay;
					mem_clk <= '1' after prop_delay;
					mem_readnotwrite <= '1' after prop_delay;
					imm_clk <= '0' after prop_delay;

				elsif (opcode = x"31") then -- LDI
					regfilein_mux <= "10" after prop_delay; -- mux selector for immediate register out
					mem_clk <= '0' after prop_delay;
					imm_clk <= '1' after prop_delay;
				end if;
				regfile_index <= destination after prop_delay;
				regfile_readnotwrite <= '0' after prop_delay;
				regfile_clk <= '1' after prop_delay;
				ir_clk <= '0' after prop_delay;
				addr_clk <= '0' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;
				pc_clk <= '0' after prop_delay, '1' after xt_prop_delay;
				pc_mux <= "00" after xt_prop_delay;
				-- We don't want to increment PC untila fter other values are propagated
				state := 1;
			when 9 => -- STO
				pc_mux <= "00" after prop_delay;
				pc_clk <= '1' after prop_delay;
				state := 10;
			when 10 =>
				memaddr_mux <= "00" after prop_delay;
				addr_mux <= '1' after prop_delay;
				regfile_clk <= '0' after prop_delay;
				mem_clk <= '1' after prop_delay;
				mem_readnotwrite <= '1' after prop_delay;
				ir_clk <= '0' after prop_delay;
				imm_clk <= '0' after prop_delay;
				addr_clk <= '1' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;
				pc_clk <= '0' after prop_delay;
				state := 11;
			when 11 =>
				memaddr_mux <= "00" after prop_delay;
				pc_mux <= "01" after prop_delay, "00" after xt_prop_delay;
				regfile_readnotwrite <= '1' after prop_delay;
				regfile_clk <= '1' after prop_delay;
				ir_clk <= '0' after prop_delay;
				imm_clk <= '0' after prop_delay;
				addr_clk <= '0' after prop_delay;
				pc_clk <= '1' after prop_delay;
				op1_clk <= '0' after prop_delay;
				op2_clk <= '0' after prop_delay;
				result_clk <= '0' after prop_delay;
				state := 1;
			when 19 =>
				pc_mux <= "00" after prop_delay;
				pc_clk <= '1' after prop_delay;
				state := 1;
			when others => null;
		   end case;
		elsif clock'event and clock = '0' then
			-- reset all the register clocks
			regfile_clk <= '0' after prop_delay;
			mem_clk <= '0' after prop_delay;
			ir_clk <= '0' after prop_delay;
			imm_clk <= '0' after prop_delay;
			addr_clk <= '0' after prop_delay;
			pc_clk <= '1' after prop_delay;
			op1_clk <= '0' after prop_delay;
			op2_clk <= '0' after prop_delay;
			result_clk <= '0' after prop_delay;
		end if;
	end process behav;
end behavior;
