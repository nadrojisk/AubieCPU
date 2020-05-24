
entity nandGate is 
	generic(prop_delay: Time := 10 ns);
	port(in1,in2: in bit;
             nandOut: out bit);
end entity nandGate; 


architecture struct of nandGate is
   signal andOut: bit; 
begin -- architecture struct
	and1: entity work.andGate(behaviour1)
	   port map (
                a_in => in1,
		b_in => in2, 
		result => andOut
	   );

        not1: entity work.notGate(behaviour1)
	   port map ( 
	       a_in => andOut,
	       result => nandOut
           );  
end architecture struct; 
