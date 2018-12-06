-- datapath_aubie.vhd

-- entity reg_file (lab 2)
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity reg_file is
  generic(prop_delay : time := 5 ns);
  port (
    data_in      : in  dlx_word;
    readnotwrite : in  bit;
    clock        : in  bit;
    data_out     : out dlx_word;
    reg_number   : in  register_index
    );
end entity reg_file;


architecture behavior of reg_file is

  type reg_type is array (0 to 31) of dlx_word;
begin
  reg_file_process : process(readnotwrite, clock, reg_number, data_in) is

    variable registers : reg_type;
  begin
    -- Start process
    if (clock = '1') then
      if (readnotwrite = '1') then
        data_out <= registers(bv_to_integer(reg_number)) after prop_delay;
      else
        registers(bv_to_integer(reg_number)) := data_in;
      end if;
    end if;
  end process reg_file_process;
end architecture behavior;


-- entity alu (lab 3)
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity alu is
  generic(prop_delay : time := 5 ns);
  port(
    operand1  : in  dlx_word;
    operand2  : in  dlx_word;
    operation : in  alu_operation_code;
    result    : out dlx_word;
    error     : out error_code
    );
end entity alu;

-- alu_operation_code values
-- 0000 unsigned add
-- 0001 signed add
-- 0010 2's compl add
-- 0011 2's compl sub
-- 0100 2's compl mul
-- 0101 2's compl divide
-- 0110 logical and
-- 0111 bitwise and
-- 1000 logical or
-- 1001 bitwise or
-- 1010 logical not (op1)
-- 1011 bitwise not (op1)
-- 1101-1111 output all zeros

-- error code values
-- 0000 = no error
-- 0001 = overflow (too big positive)
-- 0010 = underflow (too small neagative)
-- 0011 = divide by zero

architecture behaviour of alu is
begin
  alu : process(operand1, operand2, operation) is
    -- Local Variables
    variable divByZero    : boolean;
    variable tempResult   : dlx_word := x"00000000";
    variable true         : dlx_word := x"00000001";
    variable false        : dlx_word := x"00000000";
    variable OF_Flag      : boolean;
    variable inputOneTrue : bit;
    variable inputTwoTrue : bit;

  begin
    error <= "0000" after prop_delay;    -- Default Error
    case(operation) is
      when "0000" =>                     --unsigned add
    bv_addu(operand1, operand2, tempResult, OF_Flag);
    if(OF_Flag) then
      error <= "0001" after prop_delay;  -- overflow
    end if;
    result <= tempResult after prop_delay;

    when "0001" =>                      --unsigned subtract
    bv_subu(operand1, operand2, tempResult, OF_Flag);
    if(OF_Flag) then
      error <= "0010" after prop_delay;  -- subtracts do not have overflows but underflows
    end if;
    result <= tempResult after prop_delay;

    when "0010" =>                         --two's complement add
    bv_add(operand1, operand2, tempResult, OF_Flag);
    if(OF_Flag) then
                                           -- (+X) + (+Y) = (-Z)
      if ((operand1(31) = '0') and (operand2(31) = '0') and
          (tempResult(31) = '1')) then
        error <= "0001" after prop_delay;  --overflow
                                           -- (-X) + (-Y) = (+Z)
      elsif ((operand1(31) = '1') and (operand2(31) = '1') and
             (tempResult(31) = '0')) then
        error <= "0010" after prop_delay;  --underflow
      end if;
    end if;
    result <= tempResult after prop_delay;

    when "0011" =>                         --two's complement subtract
    bv_sub(operand1, operand2, tempResult, OF_Flag);
    if(OF_Flag) then
                                           -- (-X) - (+Y) = +Z
      if ((operand1(31) = '1') and (operand2(31) = '0') and
          (tempResult(31) = '0')) then
        error <= "0010" after prop_delay;  --underflow
                                           -- (+X) - (-Y) = (-Z)
      elsif ((operand1(31) = '0') and (operand2(31) = '1') and
             (tempResult(31) = '1')) then
        error <= "0001" after prop_delay;  --overflow
      end if;
    end if;
    result <= tempResult after prop_delay;

    when "0100" =>                      --two's complement multiply
    bv_mult(operand1, operand2, tempResult, OF_Flag);
    if(OF_Flag) then
                                        -- (-X) * (+Y) = (+Z)
      if ((operand1(31) = '1') and (operand2(31) = '0') and
          (tempResult(31) = '0')) then
        error <= "0010" after prop_delay;  --underflow
                                           -- (+X) * (-Y) = (+Z)
      elsif ((operand1(31) = '0') and (operand2(31) = '1') and
             (tempResult(31) = '0')) then
        error <= "0010" after prop_delay;  --underflow
      else  -- ((+X) * (+Y) = (-Z)) or ((-X ) * (-Y) = (-Z))
        error <= "0001" after prop_delay;  --overflow
      end if;
    end if;
    result <= tempResult after prop_delay;

    when "0101" =>                      --two's complement divide
    bv_div(operand1, operand2, tempResult, divByZero, OF_Flag);
    if divByZero then
      error <= "0011" after prop_delay;
    elsif OF_Flag then
      error <= "0010" after prop_delay;  -- only an underflow can occur with divide
    end if;
    result <= tempResult after prop_delay;

    when "0110" =>                      --logical and
    result       <= false;
    inputOneTrue := '0';
    inputTwoTrue := '0';
    for i in 31 downto 0 loop
      if operand1(i) = '1' then
        inputOneTrue := '1';
      end if;
      if operand2(i) = '1' then
        inputTwoTrue := '1';
      end if;
    end loop;
    if ((inputOneTrue = '1') and (inputTwoTrue = '1')) then
      result <= true after prop_delay;
    end if;


    when "0111" =>                      --bitwise and
    for i in 31 downto 0 loop
      tempResult(i) := operand1(i) and operand2(i);
    end loop;
    result <= tempResult after prop_delay;

    when "1000" =>                      --logical or
    result       <= false after prop_delay;
    inputOneTrue := '0';
    inputTwoTrue := '0';
    for i in 31 downto 0 loop
      if operand1(i) = '1' then
        inputOneTrue := '1';
      end if;
      if operand2(i) = '1' then
        inputTwoTrue := '1';
      end if;
    end loop;
    if ((inputOneTrue = '1') or (inputTwoTrue = '1')) then
      result <= true after prop_delay;
    end if;

    when "1001" =>                      --bitwise or
    for i in 31 downto 0 loop
      tempResult(i) := operand1(i) or operand2(i);
    end loop;
    result <= tempResult after prop_delay;

    when "1010" =>                      --logical not
    result       <= true after prop_delay;
    inputOneTrue := '0';
    for i in 31 downto 0 loop
      if operand1(i) = '1' then
        inputOneTrue := '1';
      end if;
    end loop;
    if (inputOneTrue = '1') then
      result <= false after prop_delay;
    end if;

    when "1011" =>                      --bitwise not
    for i in 31 downto 0 loop
      tempResult(i) := not operand1(i);
    end loop;
    result <= tempResult  after prop_delay;
    when others =>                      --1100 - 1111
    result <= x"00000000" after prop_delay;
  end case;
end process alu;
end architecture behaviour;


-- entity dlx_register (lab 3)
use work.dlx_types.all;

entity dlx_register is
  generic(prop_delay : time := 5 ns);
  port(
    in_val  : in  dlx_word;
    clock   : in  bit;
    out_val : out dlx_word
    );
end entity dlx_register;
architecture behavior of dlx_register is

begin
  dlx_reg_process : process(in_val, clock) is
  begin
    -- Start process
    if (clock = '1') then
      out_val <= in_val after prop_delay;
    end if;
  end process dlx_reg_process;
end architecture behavior;

-- entity pcplusone
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity pcplusone is
  generic(prop_delay : time := 5 ns);
  port (
    input  : in  dlx_word;
    clock  : in  bit;
    output : out dlx_word
    );
end entity pcplusone;

architecture behavior of pcplusone is
begin
  plusone : process(input, clock) is    -- add clock input to make it execute
    variable newpc : dlx_word;
    variable error : boolean;
  begin
    if clock'event and clock = '1' then
      bv_addu(input, "00000000000000000000000000000001", newpc, error);
      output <= newpc after prop_delay;
    end if;
  end process plusone;
end architecture behavior;


-- entity mux
use work.dlx_types.all;

entity mux is
  generic(prop_delay : time := 5 ns);
  port (
    input_1 : in  dlx_word;
    input_0 : in  dlx_word;
    which   : in  bit;
    output  : out dlx_word
    );
end entity mux;

architecture behavior of mux is
begin
  muxProcess : process(input_1, input_0, which) is
  begin
    if (which = '1') then
      output <= input_1 after prop_delay;
    else
      output <= input_0 after prop_delay;
    end if;
  end process muxProcess;
end architecture behavior;
-- end entity mux

-- entity threeway_mux
use work.dlx_types.all;

entity threeway_mux is
  generic(prop_delay : time := 5 ns);
  port (
    input_2 : in  dlx_word;
    input_1 : in  dlx_word;
    input_0 : in  dlx_word;
    which   : in  threeway_muxcode;
    output  : out dlx_word
    );
end entity threeway_mux;

architecture behavior of threeway_mux is
begin
  muxProcess : process(input_1, input_0, which) is
  begin
    if (which = "10" or which = "11") then
      output <= input_2 after prop_delay;
    elsif (which = "01") then
      output <= input_1 after prop_delay;
    else
      output <= input_0 after prop_delay;
    end if;
  end process muxProcess;
end architecture behavior;
-- end entity mux


-- entity memory
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity memory is
  port (
    address      : in  dlx_word;
    readnotwrite : in  bit;
    data_out     : out dlx_word;
    data_in      : in  dlx_word;
    clock        : in  bit
    );
end memory;

architecture behavior of memory is

begin  -- behavior

  mem_behav : process(address, clock) is
    -- note that there is storage only for the first 1k of the memory, to speed
    -- up the simulation
    type     memtype is array (0 to 1024) of dlx_word;
    variable data_memory : memtype;
  begin
    -- fill this in by hand to put some values in there
    -- some instructions
    data_memory(0) := X"30200000";      --LD R4, 0x100 = 256
    data_memory(1) := X"00000100";  -- address 0x100 for previous instruction
    -- R4 = Contents of Mem Addr x100 = x"5500FF00"

    data_memory(2) := X"30080000";      -- LD R1, 0x101 = 257
    data_memory(3) := X"00000101";  -- address 0x101 for previous instruction
    -- R1 = Contents of Mem Addd x101 = x"AA00FF00"


    data_memory(4) := X"30100000";      -- LD R2, 0x102 = 258
    data_memory(5) := X"00000102";  -- address 0x102 for previous instruction
    -- R2 = Contents of Mem Addr x102 = x"00000001"

    data_memory(6) := "00000000000110000100010000000000";  -- ADDU R3,R1, R2
    -- R3 = Contents of (R1 + R2) = x"AA00FF01"

    data_memory(7) := "00100000000000001100000000000000";  -- STO R3, 0x103
    data_memory(8) := x"00000103";  -- address 0x103 for previous instruction
    -- Mem Addr x"103" = data_memory(259) := contents of R3 = x"AA00FF01"


    data_memory(9)  := "00110001000000000000000000000000";  -- LDI R0, 0x104
    data_memory(10) := x"00000104";  -- #Imm value 0x104 for previous instruction
    -- Contents of R0 = x"00000104"

    data_memory(11) := "00100010000000001100000000000000";  -- STOR (R0), R3
    -- Contents of Mem Addr specifed by R0 (x104 = 260) = Contents of R3 = x"AA00FF01"

    data_memory(12) := "00110010001010000000000000000000";  -- LDR R5, (R0)
    -- Contents of R5 = Contents specified by Mem Addr[Contents of R0] = x"AA00FF01"

    data_memory(13) := x"40000000";     -- JMP to 261 = x"105"
    data_memory(14) := x"00000105";  -- Address to jump to for previous instruction
    -- JMP to Mem Addr x"105" is an Add Operation --> SUBU R11, R1, R2 => Contents of R11 = x"AA00FF01"

    -- note that this code runs every time an input signal to memory changes,
    -- so for testing, write to some other locations besides these
    data_memory(256) := "01010101000000001111111100000000";  -- x"100" = 256
    data_memory(257) := "10101010000000001111111100000000";  -- x"101" = 257
    data_memory(258) := "00000000000000000000000000000001";  -- x"102" = 258

    -- We Jumped here from Addr 14 = x"0000000E"
    data_memory(261) := x"01584400";    -- SUBU R11,R1,R2

    data_memory(262) := x"4101C000";  -- JZ R7, 267 = x"10B" -- If R7 == 0, GOTO Addr 267
    data_memory(263) := x"0000010B";  -- Address to jump to for previous instruction
    -- JZ to Mem Addr x"10B" is an Add Operation --> ADDU R12, R1 R2 => Contents of R12 = x"AA00FF01"

    -- We jumped here from Addr 263 = x"00000107"
    data_memory(267) := x"01604400";    -- SUBU R12, R1 R2

    data_memory(268) := x"10000000";    -- NOOP


    if clock = '1' then
      if readnotwrite = '1' then
        -- do a read
        data_out <= data_memory(bv_to_natural(address)) after 5 ns;
      else
        -- do a write
        data_memory(bv_to_natural(address)) := data_in;
      end if;
    end if;
  end process mem_behav;
end behavior;
-- end entity memory
