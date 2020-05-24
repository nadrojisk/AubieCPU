-- Code your testbench here
library IEEE;
use IEEE.std_logic_1164.all;


entity xorGate is
	--generic(prop_delay: Time:= 10ns);
	port(a_in, b_in, c_in: in bit;
		result: out bit);
end entity xorGate;

architecture behaviour1 of xorGate is begin 
	xorGate : process(a_in,b_in,c_in) is begin
		result <= (a_in XOR b_in) XOR c_in;-- after prop_delay;
	end process xorGate;
end architecture behaviour1;
			