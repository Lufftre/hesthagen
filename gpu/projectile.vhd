library IEEE;
    use IEEE.std_logic_1164.all;
    use IEEE.numeric_std.all;
    use IEEE.std_logic_unsigned.all;
    
entity projectile is
    port (
        CLKPROJECTILE: in std_logic;
        RST: in std_logic;
        xctr,yctr : in std_logic_vector(9 downto 0);
        xpos,ypos : in std_logic_vector(9 downto 0);
        pixel_color : out std_logic_vector(2 downto 0)
    );
end entity;

architecture rtl of projectile is 
  signal pixel_counter : integer range 0 to 15 := 1;
  signal rad : std_logic_vector(5 downto 0);
  type projectile_type is array (0 to 15) of std_logic_vector(2 downto 0);
  signal projectile : projectile_type := (
    "001","100","100","001",
    "100","001","001","100",
    "100","001","001","100",
    "001","100","100","001"
);

begin 

      -- ----------------------------------------
      --  # Fetching pixelcolor
      -- ----------------------------------------

      process(CLKPROJECTILE) begin
        if rising_edge(CLKPROJECTILE) then
            pixel_color <= projectile(pixel_counter);
            pixel_counter <= pixel_counter + 1;
        end if;
      end process;

end architecture;