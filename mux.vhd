

entity mux4bit is 
	port (select_in: in bit; in_1, in_2: in bit_vector(3 downto 0); result : out bit_vector(3 downto 0));
end entity mux4bit; 

architecture behaviour of mux4bit is 
begin
	mux_behav : process (select_in, in_1, in_2) is
	begin
		if select_in = '1' then 
			result <= in_2; 
		else 
			result <= in_1; 
		end if; 
	end process mux_behav; 
end architecture behaviour; 
	
