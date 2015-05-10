library IEEE;
    use IEEE.std_logic_1164.all;
    use IEEE.std_logic_arith.all;
    use IEEE.std_logic_unsigned.all;

--horseballs    

entity GPU is
    port (
        CLK: in std_logic;
        RST: in std_logic;
        posP1, posP2, posProj1, posProj2 : in std_logic_vector (19 downto 0);
        Hsync,Vsync : out  STD_LOGIC;
        vga : out  STD_LOGIC_VECTOR (7 downto 0); 
        NEW_FRAME : out std_logic
    );
end entity;

architecture rtl of GPU is

  -- ----------------------------------------
  -- # Components
  -- ----------------------------------------
  component board
  Port (
        CLK: in std_logic;
        RST: in std_logic;
        xctr : in std_logic_vector(9 downto 0);
        yctr : in std_logic_vector(9 downto 0);
        pixel_color : out std_logic_vector(2 downto 0)
   );
   end component;


  -- ----------------------------------------
  -- # Signals
  -- ----------------------------------------

  signal xctr,yctr : std_logic_vector(9 downto 0) := "0000000000";
  signal xpos1, ypos1 : std_logic_vector (9 downto 0);
  signal xpos2, ypos2 : std_logic_vector (9 downto 0);
  signal xposProj1, yposProj1 : std_logic_vector (9 downto 0);
  signal xposProj2, yposProj2 : std_logic_vector (9 downto 0);
            
  signal pixel : std_logic_vector(1 downto 0) := "00";
  signal pixel_color : std_logic_vector(2 downto 0);
  signal video : std_logic;
  signal hs : std_logic := '1';
  signal vs : std_logic := '1';

  signal pixel_to_vga : std_logic_vector(7 downto 0);

  type colors_type is array (0 to 7) of std_logic_vector(7 downto 0);
  signal colors : colors_type := (
    "000" & "000" & "00", -- BLACK 000
    "111" & "111" & "11", -- WHITE 001
    "110" & "000" & "00", -- RED   010
    "000" & "111" & "00", -- GREEN 011
    "000" & "000" & "11", -- BLUE  100
    "111" & "000" & "11", -- PINK  101
    "000" & "000" & "00", -- MOCHA 110
    "100" & "111" & "11" -- ICE    111
    );

  -- ----------------------------------------
  -- # GPU Architecture
  -- ----------------------------------------
begin

    b : board port map(
     CLK=>CLK,
     RST=>RST,
     xctr=>xctr,
     yctr=>yctr,
     pixel_color=>pixel_color
    );

    xpos1 <= posP1 (19 downto 10);
    ypos1 <= posP1 (9 downto 0);
    xpos2 <= posP2 (19 downto 10);
    ypos2 <= posP2 (9 downto 0);
    xposProj1 <= posProj1 (19 downto 10);
    yposProj1 <= posProj1 (9 downto 0);
    xposProj2 <= posProj2 (19 downto 10);
    yposProj2 <= posProj2 (9 downto 0);

    -- 25 MHz
    process(CLK) begin
         if rising_edge(CLK) then
           if RST='1' then
         pixel <= "00";
           else
         pixel <= pixel + 1;
           end if;
        end if;
      end process;

    -- H-sync
    process(CLK) begin
        if rising_edge(CLK) then
          if RST='1' then
             xctr <= "0000000000";
          elsif pixel=3 then
           if xctr=799 then
             xctr <= "0000000000";
           else
             xctr <= xctr + 1;
           end if;
          end if;
          -- 
          if xctr >= (639+16) and xctr <= (639+16+96) then
            hs <= '0';
          else
            hs <= '1';
          end if;
        end if;
      end process;

      -- V-Sync
      process(CLK) begin
        if rising_edge(CLK) then
          if RST='1' then
            yctr <= "0000000000";
          elsif xctr=799 and pixel=0 then
           if yctr=520 then
             yctr <= "0000000000";
             NEW_FRAME <= '1';
           else
             yctr <= yctr + 1;
             NEW_FRAME <= '0';
           end if;
           --
           if yctr >= (479+10) and yctr <= (479+10+2) then
             vs <= '0';
           else
             vs <= '1';
           end if;
          end if;
        end if;
      end process;
    
    Hsync <= hs;
    Vsync <= vs;

  process(CLK) begin
    if rising_edge(CLK) then
      if xctr > 639 or yctr > 479 then
        pixel_to_vga <= "00000000";
      else
        if xctr > xpos1 and xctr < xpos1 + 5 and yctr > ypos1 and yctr < ypos1 + 5 then
          pixel_to_vga <= "00000011";
        elsif xctr > xpos2 and xctr < xpos2 + 5 and yctr > ypos2 and yctr < ypos2 + 5 then
          pixel_to_vga <= "11100000";
        else
          pixel_to_vga <= colors(conv_integer(pixel_color));
        end if;
      end if;
    end if;
  end process;

    vga <= pixel_to_vga; 

end architecture;
