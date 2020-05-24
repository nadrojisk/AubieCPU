-- Author Jordan Sosnowski
-- Lab 3 (Registers) for Computer Architecture

use work.dlx_types.all; --custom made package types (half word, word, etc)
use work.bv_arithmetic.all; -- custom made package for arithmetic

---------------------------------- Registers -----------------------------------

-- Inputs:
-- 	One 32-bit value that defines in_val 
--	One bit value that defines clock high or low
--	One bit value that defines read or write
--	A 5-bit value that defines that register index

-- Output:
-- 	One 32-bit value that defines data_out 

-- In a given clock cycle one value can be read or one value can be written 
-- (not both). There is a 10 ns propagation delay for read operations

-- clock:
--	1 => High
--	0 => Low

-- If clock is high then values of the register can be read or set
-- If clock is low, everything is frozen until clock is high again

-- readnotwrite:
--	1 => Read
--	0 => Write

-- If readnotwrite is 1, data_in input is ignored and the value in the register
-- is copied to data_output

-- If readnotwrite is 0, the value on data_in is copied into register number
-- reg_number
--------------------------------------------------------------------------------

entity reg_file is
	generic(
		prop_delay : Time := 10 ns
	);
	port( 
		data_in: in dlx_word; 
		readnotwrite: in bit;
		clock: in bit;
		data_out: out dlx_word;
		reg_number: in register_index
	);
end entity reg_file;

architecture behaviour of reg_file is
type reg_type is array (0 to 31) of dlx_word;
begin
		reg_file : process(readnotwrite, clock, reg_number, data_in) is
		variable registers : reg_type;
		begin
				if (clock = '1') then	
					if (readnotwrite = '1') then -- Read
						data_out <= registers(bv_to_integer(reg_number)) after prop_delay;
					else
						registers(bv_to_integer(reg_number)) := data_in;
					end if;
				end if;


		end process reg_file;
end architecture behaviour;
