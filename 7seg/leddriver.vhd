library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity leddriver is
    Port ( CLK : in  STD_LOGIC;
           dp : out  STD_LOGIC;
           SEG : out STD_LOGIC_VECTOR (6 downto 0);
           AN : out  STD_LOGIC_VECTOR (3 downto 0);
           ledvalue : in  STD_LOGIC_VECTOR (15 downto 0));
end leddriver;

architecture rtl of leddriver is
	signal segments : STD_LOGIC_VECTOR (6 downto 0);
	signal counter_r :  unsigned(17 downto 0) := "000000000000000000";
	signal v : STD_LOGIC_VECTOR (3 downto 0);
begin


   dp <= '1';
	
   with counter_r(17 downto 16) select
     v <= ledvalue(15 downto 12) when "00",
          ledvalue(11 downto 8) when "01",	
          ledvalue(7 downto 4) when "10",
          ledvalue(3 downto 0) when others;

   process(CLK) begin
     if rising_edge(CLK) then 
       counter_r <= counter_r + 1;
       case v is
         when X"0" => SEG <= "1000000";
         when X"1" => SEG <= "1111001";
         when X"2" => SEG <= "0100100";
         when X"3" => SEG <= "0110000";
         when X"4" => SEG <= "0011001";
         when X"5" => SEG <= "0010010";
         when X"6" => SEG <= "0000010";
         when X"7" => SEG <= "1111000";
         when X"8" => SEG <= "0000000";
         when X"9" => SEG <= "0010000";
         when X"A" => SEG <= "0000100";
         when X"B" => SEG <= "0000011";
         when X"C" => SEG <= "1000110";
         when X"D" => SEG <= "0100001";
         when X"E" => SEG <= "0000110";
         when others => SEG <= "0001110";
       end case;
      
       case counter_r(17 downto 16) is
         when "00" => AN <= "0111";
         when "01" => AN <= "1011";
         when "10" => AN <= "1101";
         when others => AN <= "1110";
       end case;
     end if;
   end process;
	
end rtl;

