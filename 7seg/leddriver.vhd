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
         when "0000" => SEG <= "0000001";
         when "0001" => SEG <= "1001111";
         when "0010" => SEG <= "0010010";
         when "0011" => SEG <= "0000110";
         when "0100" => SEG <= "1001100";
         when "0101" => SEG <= "0100100";
         when "0110" => SEG <= "0100000";
         when "0111" => SEG <= "0001111";
         when "1000" => SEG <= "0000000";
         when "1001" => SEG <= "0000100";
         when "1010" => SEG <= "0001000";
         when "1011" => SEG <= "1100000";
         when "1100" => SEG <= "0110001";
         when "1101" => SEG <= "1000010";
         when "1110" => SEG <= "0110000";
         when others => SEG <= "0111000";
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

