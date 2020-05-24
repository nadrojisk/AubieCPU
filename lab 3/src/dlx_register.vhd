-- Author Jordan Sosnowski
-- Lab 3 (Registers) for Computer Architecture

use work.dlx_types.all; --custom made package types (half word, word, etc)
use work.bv_arithmetic.all; -- custom made package for arithmetic

----------------------------- Single Value Register ----------------------------
-- Inputs:
-- 	One 32-bit value that defines in_val 
--	One bit value that defines clock high or low

-- Output:
-- 	One 32-bit value that defines out_val 

-- In a given clock cycle one value can be read or one value can be written 
-- (not both). There is a 10 ns propagation delay for read operations

-- clock:
--	1 => High
--	0 => Low

-- If clock is high then value present at in_val will be copied to out_val
-- If clock is low, everything is frozen until clock is high again

--------------------------------------------------------------------------------

entity dlx_register is 
	generic(
		prop_delay :	Time := 10 ns
	);
	port(
		in_val	:	in  dlx_word;
		clock	:	in  bit;
		out_val	: 	out dlx_word
	);
end entity dlx_register;

architecture behavior of dlx_register is begin

	dlx_register : process(in_val, clock) is begin

		if (clock = '1') then
			out_val <= in_val after prop_delay;
		end if;

	end process dlx_register;

end architecture behavior;
