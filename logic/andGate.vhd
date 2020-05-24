entity andGate is 
	generic(prop_delay: Time := 10 ns);
	port(a_in,b_in: in bit;
             result: out bit);
end entity andGate; 


architecture behaviour1 of andGate is
begin
	andProcess : process(a_in,b_in) is 
	
	begin
                if a_in = '1' then 
                  if b_in = '1' then
                     --  1 and 1 = 1 
                     result <= '1' after prop_delay; 
                  else
                     -- 1 and 0 = 0  
                     result <= '0' after prop_delay; 
                  end if;
                else
  		  if b_in = '1' then
                     --  0 and 1 = 0
                     result <= '0' after prop_delay; 
                   else
                     -- 0 and 0 = 0  
                     result <= '0' after prop_delay; 
                   end if;  
                end if; 
	end process andProcess; 
end architecture behaviour1; 



