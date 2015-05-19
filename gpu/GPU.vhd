library IEEE;
    use IEEE.std_logic_1164.all;
    use IEEE.std_logic_arith.all;
    use IEEE.std_logic_unsigned.all;


entity GPU is
    port (
        CLK: in std_logic;
        RST: in std_logic;
        posP1, posP2, posProj1, posProj2 : in std_logic_vector (19 downto 0);
        Hsync,Vsync : out  STD_LOGIC;
        vga : out  STD_LOGIC_VECTOR (7 downto 0); 
        NEW_FRAME : out std_logic;
        xpos_int1 : in integer range 0 to 639;
        ypos_int1 : in integer range 0 to 479;
        xpos_int2 : in integer range 0 to 639;
        ypos_int2 : in integer range 0 to 479;
        proj_xpos1 : in integer range 0 to 639;
        proj_ypos1 : in integer range 0 to 479;
        proj_xpos2 : in integer range 0 to 639;
        proj_ypos2 : in integer range 0 to 479
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

  component hest
  Port (
        CLKHORSE: in std_logic;
        RST: in std_logic;
        xctr : in std_logic_vector(9 downto 0);
        yctr : in std_logic_vector(9 downto 0);
        xpos,ypos : in std_logic_vector(9 downto 0);
        pixel_color : out std_logic_vector(2 downto 0)
   );
   end component;

  component projectile
  Port (
        CLKPROJECTILE: in std_logic;
        RST: in std_logic;
        xctr : in std_logic_vector(9 downto 0);
        yctr : in std_logic_vector(9 downto 0);
        xpos,ypos : in std_logic_vector(9 downto 0);
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
  signal tile_color : std_logic_vector(2 downto 0);
  signal video : std_logic;
  signal hs : std_logic := '1';
  signal vs : std_logic := '1';

  signal pixel_to_vga : std_logic_vector(7 downto 0);

  signal CLKHORSE1 : std_logic := '0';
  signal CLKHORSE2 : std_logic := '0';
  signal CLKPROJECTILE1 : std_logic := '0';
  signal CLKPROJECTILE2 : std_logic := '0';
  signal hest_color1 : std_logic_vector(2 downto 0);
  signal hest_color2 : std_logic_vector(2 downto 0);
  signal projectile_color1 : std_logic_vector(2 downto 0);
  signal projectile_color2 : std_logic_vector(2 downto 0);

  type colors_type is array (0 to 7) of std_logic_vector(7 downto 0);
  signal colors : colors_type := (
    "000" & "000" & "00", -- BLACK 000
    "111" & "111" & "11", -- WHITE 001
    "110" & "000" & "00", -- RED   010
    "000" & "111" & "00", -- GREEN 011
    "000" & "000" & "11", -- BLUE  100
    "111" & "000" & "11", -- PINK  101
    "011" & "001" & "00", -- MOCHA 110
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
     pixel_color=>tile_color
    );

    h1 : hest port map(
     CLKHORSE=>CLKHORSE1,
     RST=>RST,
     xctr=>xctr,
     yctr=>yctr,
     xpos=>xpos1,
     ypos=>ypos1,
     pixel_color=>hest_color1
    );

    h2 : hest port map(
     CLKHORSE=>CLKHORSE2,
     RST=>RST,
     xctr=>xctr,
     yctr=>yctr,
     xpos=>xpos2,
     ypos=>ypos2,
     pixel_color=>hest_color2
    );

    p1 : projectile port map(
     CLKPROJECTILE1=>CLKPROJECTILE1,
     RST=>RST,
     xctr=>xctr,
     yctr=>yctr,
     xpos=>xpos1,
     ypos=>ypos1,
     pixel_color=>projectile_color1
    );

    p2 : projectile port map(
     CLKPROJECTILE2=>CLKPROJECTILE2,
     RST=>RST,
     xctr=>xctr,
     yctr=>yctr,
     xpos=>xpos2,
     ypos=>ypos2,
     pixel_color=>projectile_color2
    );

    xpos1 <= posP1 (19 downto 10);
    ypos1 <= posP1 (9 downto 0);
    xpos2 <= posP2 (19 downto 10);
    ypos2 <= posP2 (9 downto 0);
    xposProj1 <= posProj1 (19 downto 10);
    yposProj1 <= posProj1 (9 downto 0);
    xposProj2 <= posProj2 (19 downto 10);
    yposProj2 <= posProj2 (9 downto 0);


  -- ----------------------------------------
  -- # Yolo comment
  -- ----------------------------------------
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
      CLKHORSE1 <= '0';
      CLKHORSE2 <= '0';
      if pixel=3 then
        if xctr > 639 or yctr > 479 then
          pixel_to_vga <= "00000000";
        else
          pixel_to_vga <= colors(conv_integer(tile_color));

          if xctr >= xpos_int1 and xctr < xpos_int1 + 16 and yctr >= ypos_int1 and yctr < ypos_int1 + 16 then
            CLKHORSE1 <= '1';
            if hest_color1 /= "111" then
              pixel_to_vga <= colors(conv_integer(hest_color1));
            end if;
          end if;
          if xctr >= xpos_int2 and xctr < xpos_int2 + 16 and yctr >= ypos_int2 and yctr < ypos_int2 + 16 then
            CLKHORSE2 <= '1';
            if hest_color2 /= "111" then
              pixel_to_vga <= colors(conv_integer(hest_color2));
            end if;
          end if;

          if xctr >= proj_xpos1 and xctr < proj_xpos1 + 16 and yctr >= proj_ypos1 and yctr < proj_ypos1 + 16 then
            CLKPROJECTILE1 <= '1';
            if projectile_color1 /= "111" then
              pixel_to_vga <= colors(conv_integer(projectile_color1));
            end if;
          end if;

          if xctr >= proj_xpos2 and xctr < proj_xpos2 + 16 and yctr >= proj_ypos2 and yctr < proj_ypos2 + 16 then
            CLKPROJECTILE2 <= '1';
            if projectile_color2 /= "111" then
              pixel_to_vga <= colors(conv_integer(projectile_color2));
            end if;
          end if;
            
            
        end if;
      end if;
    end if;
  end process;
  vga <= pixel_to_vga; 

end architecture;