-- Author Jordan Sosnowski
-- Lab 4 (CPU) for Computer Architecture

use work.bv_arithmetic.all;
use work.dlx_types.all;



entity aubie_controller is
  generic(
    prop_delay    : time := 5 ns;
    ex_prop_delay : time := 15 ns  -- allows other signals to propagate first
    );
  port(
    ir_control           : in  dlx_word;
    alu_out              : in  dlx_word;
    alu_error            : in  error_code;
    clock                : in  bit;
    regfilein_mux        : out threeway_muxcode;
    memaddr_mux          : out threeway_muxcode;
    addr_mux             : out bit;
    pc_mux               : out threeway_muxcode;
    alu_func             : out alu_operation_code;
    regfile_index        : out register_index;
    regfile_readnotwrite : out bit;
    regfile_clk          : out bit;
    mem_clk              : out bit;
    mem_readnotwrite     : out bit;
    ir_clk               : out bit;
    imm_clk              : out bit;
    addr_clk             : out bit;
    pc_clk               : out bit;
    op1_clk              : out bit;
    op2_clk              : out bit;
    result_clk           : out bit
    );
end aubie_controller;

architecture behavior of aubie_controller is
begin
  behav : process(clock) is
    type     state_type is range 1 to 20;
    variable state                           : state_type         := 1;
    variable opcode                          : byte;
    variable destination, operand1, operand2 : register_index;
    variable op_stor                         : alu_operation_code := "0111";
    variable jz_op                           : alu_operation_code := "1100";
    variable logical_true                    : dlx_word           := x"00000001";



  begin
    if clock'event and clock = '1' then
      opcode      := ir_control(31 downto 24);
      destination := ir_control(23 downto 19);
      operand1    := ir_control(18 downto 14);
      operand2    := ir_control(13 downto 9);
      case state is
        when 1 =>                       -- fetch the instruction, for all types
          memaddr_mux      <= "00" after prop_delay;  -- memory threeway_mux input_0 to read from PC
          regfile_clk      <= '0'  after prop_delay;
          mem_clk          <= '1'  after prop_delay;
          mem_readnotwrite <= '1'  after prop_delay;  -- reading instruction from mem
          ir_clk           <= '1'  after prop_delay;  -- High so Instruction Register can receive a signal from Memory
          imm_clk          <= '0'  after prop_delay;
          addr_clk         <= '0'  after prop_delay;
          addr_mux         <= '1'  after prop_delay;
          pc_clk           <= '0'  after prop_delay;  -- Low so program counter will output the current address it retains
          op1_clk          <= '0'  after prop_delay;
          op2_clk          <= '0'  after prop_delay;
          result_clk       <= '0'  after prop_delay;
          state            := 2;
        when 2 =>   -- figure out which operation
          if opcode(7 downto 4) = "0000" then           -- ALU op
            state := 3;
          elsif opcode = X"20" then                     -- STO
            state := 9;
          elsif opcode = X"30" or opcode = X"31" then   -- LD or LDI
            state := 7;
          elsif opcode = X"22" then                     -- STOR
            state := 14;
          elsif opcode = X"32" then                     -- LDR
            state := 12;
          elsif opcode = X"40" or opcode = X"41" then   -- JMP or JZ
            state := 16;
          elsif opcode = X"10" then                     -- NOOP
            state := 19;
          else                                          -- error
          end if;
        when 3 =>  -- ALU op (Step 1)
          -- load operand 1 register from the regfile to op1
          regfile_index        <= operand1 after prop_delay;
          regfile_readnotwrite <= '1'      after prop_delay;  -- reading op from regfile
          regfile_clk          <= '1'      after prop_delay;  -- high so register can receive value
          mem_clk              <= '0'      after prop_delay;
          ir_clk               <= '0'      after prop_delay;
          imm_clk              <= '0'      after prop_delay;
          addr_clk             <= '0'      after prop_delay;
          pc_clk               <= '0'      after prop_delay;
          op1_clk              <= '1'      after prop_delay;  -- needs to be high so it can accept regfile data
          op2_clk              <= '0'      after prop_delay;
          result_clk           <= '0'      after prop_delay;
          state                := 4;
        when 4 =>   -- ALU op (Step 2)
          -- load op2 registear from the regfile
          regfile_index        <= operand2 after prop_delay;
          regfile_readnotwrite <= '1'      after prop_delay;
          regfile_clk          <= '1'      after prop_delay;
          mem_clk              <= '0'      after prop_delay;
          ir_clk               <= '0'      after prop_delay;
          imm_clk              <= '0'      after prop_delay;
          addr_clk             <= '0'      after prop_delay;
          pc_clk               <= '0'      after prop_delay;
          op1_clk              <= '0'      after prop_delay;
          op2_clk              <= '1'      after prop_delay;  -- needs to be high so it can accept regfile data
          result_clk           <= '0'      after prop_delay;
          state                := 5;
        when 5 =>    -- ALU op (Step 3)
          -- Perform ALU operation
          alu_func    <= opcode(3 downto 0) after prop_delay;
          regfile_clk <= '0'                after prop_delay;
          mem_clk     <= '0'                after prop_delay;
          ir_clk      <= '0'                after prop_delay;
          imm_clk     <= '0'                after prop_delay;
          addr_clk    <= '0'                after prop_delay;
          pc_clk      <= '0'                after prop_delay;
          op1_clk     <= '0'                after prop_delay;
          op2_clk     <= '0'                after prop_delay;
          result_clk  <= '1'                after prop_delay;  -- high so it can accept alu data
          state       := 6;
        when 6 =>    -- ALU op (Step 4)
          -- Write back ALU result
          regfilein_mux        <= "00"        after prop_delay;  -- selects result
          pc_mux               <= "00"        after prop_delay;  -- selects pcplusone_out
          regfile_index        <= destination after prop_delay;
          regfile_readnotwrite <= '0'         after prop_delay;  -- Write back to destination
          regfile_clk          <= '1'         after prop_delay;
          ir_clk               <= '0'         after prop_delay;
          imm_clk              <= '0'         after prop_delay;
          addr_clk             <= '0'         after prop_delay;
          pc_clk               <= '1'         after prop_delay;  -- To increment PC
          op1_clk              <= '0'         after prop_delay;
          op2_clk              <= '0'         after prop_delay;
          result_clk           <= '0'         after prop_delay;
          state                := 1;
        when 7 =>    -- LD / LDI (Step 1)
          if (opcode = x"30") then    -- LD
            -- load contents of address to register destination
            -- Increment PC. Copy memory specified by PC into address register
            pc_clk           <= '1'  after prop_delay;
            pc_mux           <= "00" after prop_delay;  -- pcplusone_out
            memaddr_mux      <= "00" after prop_delay;  -- mux select read from pcplusone_out
            addr_mux         <= '1'  after prop_delay;  -- input_1 select of mem_out
            regfile_clk      <= '0'  after prop_delay;
            mem_clk          <= '1'  after prop_delay;
            mem_readnotwrite <= '1'  after prop_delay;  -- Memory Read operation
            ir_clk           <= '0'  after prop_delay;
            imm_clk          <= '0'  after prop_delay;
            addr_clk         <= '1'  after prop_delay;
            op1_clk          <= '0'  after prop_delay;
            op2_clk          <= '0'  after prop_delay;
            result_clk       <= '0'  after prop_delay;
          elsif (opcode = x"31") then    -- LDI
            -- load immediate value into register destination
            -- Increment PC. Copy memory specified by PC into immediate register
            pc_clk           <= '1'  after prop_delay;
            pc_mux           <= "00" after prop_delay;  -- pcplusone_out
            memaddr_mux      <= "00" after prop_delay;
            regfile_clk      <= '0'  after prop_delay;
            mem_clk          <= '1'  after prop_delay;
            mem_readnotwrite <= '1'  after prop_delay;
            ir_clk           <= '0'  after prop_delay;
            imm_clk          <= '1'  after prop_delay;
            addr_clk         <= '0'  after prop_delay;
            op1_clk          <= '0'  after prop_delay;
            op2_clk          <= '0'  after prop_delay;
            result_clk       <= '0'  after prop_delay;
          end if;
          state := 8;
        when 8 =>    -- LD / LDI (Step 2)
          -- Copy value to register
          if (opcode = x"30") then    -- LD
            regfilein_mux    <= "01" after prop_delay;
            memaddr_mux      <= "01" after prop_delay;
            mem_clk          <= '1'  after prop_delay;
            mem_readnotwrite <= '1'  after prop_delay;
            imm_clk          <= '0'  after prop_delay;
          elsif (opcode = x"31") then    -- LDI
            regfilein_mux <= "10" after prop_delay;  -- mux selector for immediate register out
            mem_clk       <= '0'  after prop_delay;
            imm_clk       <= '1'  after prop_delay;
          end if;

          regfile_index        <= destination after prop_delay;
          regfile_readnotwrite <= '0'         after prop_delay;
          regfile_clk          <= '1'         after prop_delay;
          ir_clk               <= '0'         after prop_delay;
          addr_clk             <= '0'         after prop_delay;
          op1_clk              <= '0'         after prop_delay;
          op2_clk              <= '0'         after prop_delay;
          result_clk           <= '0'         after prop_delay;
          pc_clk               <= '0'         after prop_delay, '1' after ex_prop_delay;
          pc_mux               <= "00"        after ex_prop_delay;
          -- Don't want to increment PC until after other values are propagated
          state                := 1;
        when 9 =>    -- STO (Step 1)
          -- Increment PC
          pc_mux <= "00" after prop_delay;
          pc_clk <= '1'  after prop_delay;
          state  := 10;
        when 10 =>    -- STO (Step 2)
          -- Load memory to address register
          memaddr_mux      <= "00" after prop_delay;
          addr_mux         <= '1'  after prop_delay;
          regfile_clk      <= '0'  after prop_delay;
          mem_clk          <= '1'  after prop_delay;
          mem_readnotwrite <= '1'  after prop_delay;
          ir_clk           <= '0'  after prop_delay;
          imm_clk          <= '0'  after prop_delay;
          addr_clk         <= '1'  after prop_delay;
          op1_clk          <= '0'  after prop_delay;
          op2_clk          <= '0'  after prop_delay;
          result_clk       <= '0'  after prop_delay;
          pc_clk           <= '0'  after prop_delay;
          state            := 11;
        when 11 =>    -- STO (Step 3)
          -- Store contents of source to address in memory given by addr reg
          -- then increment pc
          memaddr_mux          <= "00" after prop_delay;
          pc_mux               <= "01" after prop_delay, "00" after ex_prop_delay;
          regfile_readnotwrite <= '1'  after prop_delay;
          regfile_clk          <= '1'  after prop_delay;
          ir_clk               <= '0'  after prop_delay;
          imm_clk              <= '0'  after prop_delay;
          addr_clk             <= '0'  after prop_delay;
          pc_clk               <= '1'  after prop_delay;
          op1_clk              <= '0'  after prop_delay;
          op2_clk              <= '0'  after prop_delay;
          result_clk           <= '0'  after prop_delay;
          state                := 1;
        when 12 =>    -- LDR (Step 1)
          -- Copy contents of operand1 reg to addr reg
          addr_mux             <= '0'      after prop_delay;
          regfile_index        <= operand1 after prop_delay;
          regfile_readnotwrite <= '1'      after prop_delay;
          regfile_clk          <= '1'      after prop_delay;
          mem_clk              <= '0'      after prop_delay;
          ir_clk               <= '0'      after prop_delay;
          imm_clk              <= '0'      after prop_delay;
          addr_clk             <= '1'      after prop_delay;
          pc_clk               <= '0'      after prop_delay;
          op1_clk              <= '0'      after prop_delay;
          op2_clk              <= '0'      after prop_delay;
          result_clk           <= '0'      after prop_delay;
          state                := 13;
        when 13 =>    -- LDR (Step 2)
          -- copy contetns of memory specified by addr reg to dest reg
          regfilein_mux        <= "01"        after prop_delay;
          memaddr_mux          <= "01"        after prop_delay;
          regfile_index        <= destination after prop_delay;
          regfile_readnotwrite <= '0'         after prop_delay;
          regfile_clk          <= '1'         after prop_delay;
          mem_clk              <= '1'         after prop_delay;
          mem_readnotwrite     <= '1'         after prop_delay;
          ir_clk               <= '0'         after prop_delay;
          imm_clk              <= '0'         after prop_delay;
          addr_clk             <= '0'         after prop_delay;
          pc_clk               <= '0'         after prop_delay;
          op1_clk              <= '0'         after prop_delay;
          op2_clk              <= '0'         after prop_delay;
          result_clk           <= '0'         after prop_delay;
          pc_clk               <= '0'         after prop_delay, '1' after ex_prop_delay;
          pc_mux               <= "00"        after ex_prop_delay;
          state                := 1;
        when 14 =>    -- STOR (Step 1)
          -- Copy contents of dest reg into addr reg
          addr_mux             <= '0'         after prop_delay;
          regfile_index        <= destination after prop_delay;
          regfile_readnotwrite <= '1'         after prop_delay;
          regfile_clk          <= '1'         after prop_delay;
          mem_clk              <= '0'         after prop_delay;
          ir_clk               <= '0'         after prop_delay;
          imm_clk              <= '0'         after prop_delay;
          addr_clk             <= '1'         after prop_delay;
          pc_clk               <= '0'         after prop_delay;
          op1_clk              <= '0'         after prop_delay;
          op2_clk              <= '0'         after prop_delay;
          result_clk           <= '0'         after prop_delay;
          state                := 15;
        when 15 =>    -- STOR (Step 2)
          -- Copy contents of operand1 reg to memory address
          memaddr_mux          <= "00"     after prop_delay;
          pc_mux               <= "01"     after prop_delay, "00" after ex_prop_delay;
          alu_func             <= op_stor  after prop_delay;
          regfile_index        <= operand1 after prop_delay;
          regfile_readnotwrite <= '1'      after prop_delay;
          regfile_clk          <= '1'      after prop_delay;
          mem_clk              <= '1'      after prop_delay;
          mem_readnotwrite     <= '0'      after prop_delay;
          ir_clk               <= '0'      after prop_delay;
          imm_clk              <= '0'      after prop_delay;
          addr_clk             <= '0'      after prop_delay;
          pc_clk               <= '1'      after prop_delay;
          op1_clk              <= '1'      after prop_delay;
          op2_clk              <= '1'      after prop_delay;
          result_clk           <= '1'      after prop_delay;
          state                := 1;
        when 16 =>    -- JMP / JZ (Step 1)
          -- Increment PC --> PC+1
          pc_mux <= "00" after prop_delay;
          pc_clk <= '1'  after prop_delay;
          state  := 17;
        when 17 =>    -- JMP or JZ (Step2):
          -- Load memory specified by PC to Address register: Mem[PC] --> Addr
          -- Same thing as State 7 except no need to increment since that was already done in State 16
          pc_clk           <= '0'  after prop_delay;
          memaddr_mux      <= "00" after prop_delay;  -- mux select read from pcplusone_out
          addr_mux         <= '1'  after prop_delay;  -- input_1 select of mem_out
          regfile_clk      <= '0'  after prop_delay;
          mem_clk          <= '1'  after prop_delay;
          mem_readnotwrite <= '1'  after prop_delay;  -- Memory Read operation
          ir_clk           <= '0'  after prop_delay;
          imm_clk          <= '0'  after prop_delay;
          addr_clk         <= '1'  after prop_delay;
          op1_clk          <= '0'  after prop_delay;
          op2_clk          <= '0'  after prop_delay;
          result_clk       <= '0'  after prop_delay;
          state            := 18;
          if (opcode = x"40") then    -- JMP
            state := 18;
          else    -- JZ Intermediate Step to Check if OP1 == 0
            state := 20;
          end if;
        when 18 =>    -- JMP or JZ (Step3):
          if (opcode = x"40") then    -- JMP
            -- Load Addr to PC
            pc_mux <= "01" after prop_delay;
            pc_clk <= '1'  after prop_delay;
          end if;
          if (opcode = x"41") then    -- JZ
            -- If Result == 0, copy Addr to PC:  else increment PC
            if (alu_out = logical_true) then
              pc_mux <= "01" after prop_delay;
              pc_clk <= '1'  after prop_delay;
            else
              pc_clk <= '1'  after prop_delay;
              pc_mux <= "00" after prop_delay;
            end if;
          end if;
          state := 1;
        when 19 =>    -- NOOP: Only increments PC
          pc_mux <= "00" after prop_delay;
          pc_clk <= '1'  after prop_delay;

          state := 1;
        when 20 =>    -- JZ Intermediate Cycle
          -- copy register op1 to control
          alu_func             <= jz_op    after prop_delay;
          regfile_index        <= operand1 after prop_delay;
          regfile_readnotwrite <= '1'      after prop_delay;
          regfile_clk          <= '1'      after prop_delay;
          mem_clk              <= '0'      after prop_delay;
          ir_clk               <= '0'      after prop_delay;
          imm_clk              <= '0'      after prop_delay;
          addr_clk             <= '0'      after prop_delay;
          pc_clk               <= '0'      after prop_delay;
          op1_clk              <= '1'      after prop_delay;
          op2_clk              <= '1'      after prop_delay;
          result_clk           <= '1'      after prop_delay;
          state                := 18;
        when others => null;
      end case;
    elsif clock'event and clock = '0' then
      -- reset all the register clocks
      regfile_clk <= '0' after prop_delay;
      mem_clk     <= '0' after prop_delay;
      ir_clk      <= '0' after prop_delay;
      imm_clk     <= '0' after prop_delay;
      addr_clk    <= '0' after prop_delay;
      pc_clk      <= '0' after prop_delay;
      op1_clk     <= '0' after prop_delay;
      op2_clk     <= '0' after prop_delay;
      result_clk  <= '0' after prop_delay;
    end if;
  end process behav;
end behavior;
