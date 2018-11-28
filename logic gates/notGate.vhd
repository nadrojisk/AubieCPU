entity notGate is 
	generic(prop_delay: Time := 10 ns);
	port(a_in: in bit;
             result: out bit);
end entity notGate; 


architecture behaviour1 of notGate is
begin
	notProcess : process(a_in) is 
	
	begin
                if a_in = '1' then 
                   result <= '0' after prop_delay; 
                else 
                     result <= '1' after prop_delay; 
                end if;
	end process notProcess; 
end architecture behaviour1; 

