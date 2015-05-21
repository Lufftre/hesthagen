library IEEE;
library ieee_proposed;
  use ieee_proposed.fixed_pkg.all;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity CPU is
    Port ( 
    CLK : in  STD_LOGIC;
    RST : in  STD_LOGIC;
    NEW_FRAME : in std_logic;
    joystick1 : in  STD_LOGIC_VECTOR (39 downto 0);
    joystick2 : in  STD_LOGIC_VECTOR (39 downto 0);
    mem : out std_logic_vector(15 downto 0);
    xpos_int1 : out integer range 0 to 639;
    ypos_int1 : out integer range 0 to 479;
    xpos_int2 : out integer range 0 to 639;
    ypos_int2 : out integer range 0 to 479;
    proj_xpos1 : out integer range 0 to 639;
    proj_ypos1 : out integer range 0 to 479;
    proj_xpos2 : out integer range 0 to 639;
    proj_ypos2 : out integer range 0 to 479;
    horse_tile1 : in std_logic_vector(2 downto 0);
    horse_tile2 : in std_logic_vector(2 downto 0);
    current_map : out std_logic_vector(1 downto 0)
    );
end entity;
architecture rtl of CPU is
    -- Register
    signal ASR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal PC_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal AR_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal HR_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal IR_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal GR0_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- Player1 HÄLSA
    signal GR1_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- PLayer2 HÄLSA 
    signal GR2_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- MAP COUNTER
    signal GR3_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- MAP
    signal flag_newframe : STD_LOGIC := '0';
    signal z_flag : STD_LOGIC := '0';
    
    -- PM/RAM och MyM
    type ram_type is array (0 to 34) of std_logic_vector(15 downto 0);
    type mram_type is array (0 to 45) of std_logic_vector(24 downto 0);
    


    signal ram : ram_type := (
    -- Programkod
    X"301A", --00 ADD PLAYER 1 HP
    X"381A", --01 ADD PLAYER 2 HP
    X"3818", --02 ADD MAP_COUNTER MAP_VALUE
    X"901E", --03 STAY UNTIL NEW FRAME
    X"A000", --04 MOVE PLAYERS
    X"B000", --05 MOVE PROJECTILES
    X"C000", --06 HANDLE COLLISION
    X"D01F", --07 GET BURNING HORSE
    X"E01F", --08 BTST 1F
    X"F01C", --09 BNE HOPPA PLAYER 2 
    X"4020", --0A SUB PLAYER_1_HÄLSA - 1
    X"F01D", --0B BNE GAMEOVER  
    X"501F", --0C LSR 1F                PLAYER 2 HOPP
    X"E01F", --0D BTST 1F
    X"F01E", --0E BNE START
    X"4420", --0F SUB PLAYER_2_HÄLSA
    X"F01D", --10 BNE GAMEOVER
    X"4820", --11 SUB MAP_COUNTER
    X"F021", --12 BNE NEW MAP
    X"601E", --13 JMP START
    X"3C20", --14 ADD MAP 20            NEW MAP 
    X"3818", --15 ADD MAP_COUNTER MAP_VALUE
    X"601E", --16 JMP START
    X"0000", --17 GAMEOVER
    X"00FF", --18 MAP VALUE
    X"0000", --19 
    X"0000", --20
    X"00FF", --1B START HP
    X"000C", --1C PLAYER2 HOPP ADDRESS
    X"0017", --1D GAME OVER ADDRESS
    X"0003", --1E START ADDRESS
    X"0000", --1F BURNING HORSE?
    X"0001", --20 ETTA
    X"0000", --21 NEW MAP ADRESS
    X"0000"
    );

    --signal ram : ram_type := (
    ---- Programkod
    --X"301A", --00 ADD PLAYER 1 HP
    --X"381A", --01 ADD PLAYER 2 HP
    --X"901D", --02 STAY UNTIL NEW FRAME
    --X"A000", --03 MOVE PLAYERS
    --X"B000", --04 MOVE PROJECTILES
    --X"C000", --05 HANDLE COLLISION
    --X"D01E", --06 GET BURNING HORSE
    --X"E01E", --07 BTST 1E
    --X"F01D", --08 BNE HOPPA PLAYER 2 
    --X"401F", --09 SUB PLAYER_1_HÄLSA - 1
    --X"601D", --07 JMP START
    --X"601C", --0B JMP gAMEOVER
    --X"501E", --0C LSR 1E       PLAYER 2 HOPP
    --X"E01E", --0D BTST 1E
    --X"F01D", --0E BNE START
    --X"481F", --0F SUB PLAYER_2_HÄLSA
    --X"F01D", --10 BNE START
    --X"0000", --11 GAMEOVER
    --X"0000", --12 GAMEOVER
    --X"0000", --13     
    --X"0000", --14
    --X"0000", --15
    --X"0000", --16
    --X"0000", --17
    --X"0000", --18 
    --X"0000", --19 
    --X"000F", --1A START HP
    --X"000C", --1B PLAYER2 HOPP ADDRESS
    --X"0011", --1C GAME OVER ADDRESS
    --X"0002", --1D START ADDRESS
    --X"0000", --1E BURNING HORSE?
    --X"0001"  --1F ETTA
    --);


    constant mram : mram_type := (
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "011" & "111" & "0" & "0" & "00" & "0000" & "0000000", --0x00 PC => ASR, mpc++,                        (HÄMTFAS)
    "0000" & "010" & "001" & "0" & "0" & "00" & "0000" & "0000000", --0x01 PM => IR, mpc++ 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0010" & "0000000", --0x02 K1 => mpc
    "0000" & "001" & "111" & "0" & "0" & "00" & "0001" & "0000000", --0x03 IR => ASR, K2 => mpc
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x04
    "0000" & "010" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x05 PM => GRx, mpc = 0,                      (LOAD)
    "0000" & "110" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x06 GRx => PM, mpc = 0,                      (STORE)
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x07 Grx => AR, mpc++,                        (ADD)
    "1000" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x08 (AR + PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x09 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0A Grx => AR, mpc++,                        (SUB)
    "0101" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0B (AR - PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x0C AR => GRx, PC++, mpc = 0
    "0001" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0D PM => AR, mpc++,                        (LSR)
    "1110" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0E (AR LSR 1) => AR , mpc++
    "0000" & "100" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x0F AR => PM, PC++, mpc = 0
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "010" & "011" & "0" & "0" & "00" & "0011" & "0000000", --0x10 PM => PC, mpc = 0                        (JMP)
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x11 Grx => AR, mpc++,                        (XOR 0x0800)
    "1111" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x12 (NOT AR(10) ) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x13 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x14 GRx => AR, mpc++                         (AND)
    "0010" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x15 ALU magic, mp++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x16 AR => GRx, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0011" & "0000000", --0x17 mpc = 0                                  (NOP)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1000" & "0010000", --0x18 IF flagga = 1 then mpc++ else mpc =>myADR (B??)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x19 PC++, mpc = 0
    "1001" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1A Grx => AR, mpc++,                        (SUPER MOVE)
    "1011" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1B Grx => AR, mpc++,                        (SUPER PEWPEW)
    "1100" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1C Grx => AR, mpc++,                        (SUPER HIT)
    "1101" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1D Grx => AR, mpc++,                        (SUPER LAVA)
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x1E
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000",  --0x1F
--    ALU     TB      FB      S     P    LC      SEQ       myADR      
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x20 AR => PM, PC++, mpc = 0                  (Flag on burning horse) 
    "1111" & "010" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x21 BTST PM, PC++, mpc = 0                   (BTST)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1001" & "0010000", --0x22 IF z = 1 then mpc++ else mpc=>myADR      (BNE)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x23 PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x24 
    "1101" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x25 Alu magic => AR, mpc ++
    "0000" & "100" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x26 AR => PM, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x27 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x28 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x29 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2A
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2B
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2E 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000"  --0x2F
    );
    
    -- K1 och K2
    type k1_type is array(0 to 15) of std_logic_vector(7 downto 0);
    type k2_type is array(0 to 3) of std_logic_vector(7 downto 0);

    constant k1 : k1_type := (
    X"00", -- NOP    0x00
    X"05", -- LOAD   0x01
    X"06", -- STORE  0x02   
    X"07", -- ADD    0x03
    X"0A", -- SUB    0x04
    X"0D", -- LSR    0x05
    X"10", -- JMP    0x06
    X"11", -- XOR    0x07
    X"14", -- AND    0x08
    X"18", -- B??    0x09
    X"1A", -- MOVE   0x0A
    X"1B", -- PEW    0x0B
    X"1C", -- HIT    0x0C
    X"25", -- GBH    0x0D  (GET BURNING HORSE) 
    X"21", -- BTST   0x0E
    X"22"  -- BNE    0x0F
    );
    constant k2 : k2_type := (
    X"03",
    X"00",
    X"00",
    X"00"
    );
    -- Interna signaler
    signal buss : std_logic_vector(15 downto 0) := X"0000";
    signal mux1 : std_logic_vector(1 downto 0) := "00"; --Mux för de 4 GR register
    signal current_GR : std_logic_vector(15 downto 0);
    
    signal MPC : STD_LOGIC_VECTOR(7 downto 0) := X"00";
    signal myM : std_logic_vector(24 downto 0);
    -- Mym operatorer
    signal ALU_OP : std_logic_vector(3 downto 0) := "0000"; --Best�mmer operator i ALU
    signal TB : std_logic_vector(2 downto 0) := "000";
    signal FB : std_logic_vector(2 downto 0) := "000";
    signal S : std_logic;
    signal P : std_logic;
    signal LC : std_logic_vector(1 downto 0);
    signal SEQ : std_logic_vector(3 downto 0);
    signal myADR : std_logic_vector(6 downto 0) := "0000000";



    -- PLAYER MOVE SIGNALS

    -- PLAYER 1
    --x
    signal vel_x1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal delta_x1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal xpos_real1 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_x1 : std_logic_vector(9 downto 0);
    --y
    signal vel_y1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal delta_y1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal ypos_real1 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_y1 : std_logic_vector(9 downto 0);
    --PLAYER 2
    --x
    signal vel_x2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal delta_x2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal xpos_real2 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_x2 : std_logic_vector(9 downto 0);
    --y
    signal vel_y2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal delta_y2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal ypos_real2 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_y2 : std_logic_vector(9 downto 0);


    -- PROJECTILE SIGNALS

    --PROJ 1
    signal proj_active1 : std_logic := '0';
    --x
    signal proj_dirx1 : std_logic_vector(9 downto 0);
    signal proj_deltax1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_real_xpos1 : sfixed(9 downto -4) := to_sfixed(120, 9, -4);

    --y
    signal proj_diry1 : std_logic_vector(9 downto 0);
    signal proj_deltay1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_real_ypos1 : sfixed(9 downto -4) := to_sfixed(120, 9, -4);

    
    --PROJ 2
    signal proj_active2 : std_logic := '0';
    --x
    signal proj_dirx2 : std_logic_vector(9 downto 0);
    signal proj_deltax2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_real_xpos2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4);
    --y
    signal proj_diry2 : std_logic_vector(9 downto 0);
    signal proj_deltay2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_real_ypos2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4);    



    signal hit_counter1 : integer range 0 to 15 := 0; -- Knockback duration player 1
    signal hit_counter2 : integer range 0 to 15 := 0; -- Knockback duration player 2

    signal hp1 : integer range 0 to 63 := 63; -- Healthpoints player 1
    signal hp2 : integer range 0 to 63 := 63; -- Healthpoints player 2

    signal map_counter : std_logic_vector(7 downto 0) := X"00"; --Timer for map cycle
    signal cur_map : std_logic_vector(1 downto 0) := "00"; -- Current map

begin
    flag_newframe <= NEW_FRAME;
    current_map <= cur_map;
    mem <= ram(30);
    current_map <= GR3_REG(1 downto 0);
    -- ----------------------------------------
    -- # ASR Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
        if (FB = "111") then
                ASR_REG(15 downto 0) <= buss(15 downto 0);
            else
                ASR_REG(15 downto 0) <= ASR_REG(15 downto 0);
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # PM
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
            if FB="010" then
                ram(conv_integer(ASR_REG(8 downto 0))) <= buss(15 downto 0);
            else 
                ram <= ram;
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # IR Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
        if FB="001" then
                IR_REG(15 downto 0) <= buss(15 downto 0);
            else
                IR_REG(15 downto 0) <= IR_REG(15 downto 0);
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # PC Register
    -- ----------------------------------------
     process(CLK) begin
         if rising_edge(CLK) then
            if FB="011" then
                 PC_REG(15 downto 0) <= buss(15 downto 0);
            elsif (P = '1') then
                PC_REG <= PC_reg + 1;
             else
                 PC_REG(15 downto 0) <= PC_REG(15 downto 0);
             end if;
          end if;
      end process;         
 
    -- ----------------------------------------
    -- # HR Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
        if FB="101" then
                HR_REG(15 downto 0) <= buss(15 downto 0);
            else
                HR_REG(15 downto 0) <= HR_REG(15 downto 0);
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # MUX1 Register
    -- ----------------------------------------
    mux1 <= IR_REG(11 downto 10);
    process(CLK) begin
        if rising_edge(CLK) then
        if FB="110" then
                case mux1 is
                    when "00" => GR0_REG(15 downto 0) <= buss(15 downto 0);
                    when "01" => GR1_REG(15 downto 0) <= buss(15 downto 0);
                    when "10" => GR2_REG(15 downto 0) <= buss(15 downto 0);
                    when "11" => GR3_REG(15 downto 0) <= buss(15 downto 0);
                    when others => null;
                end case;
            end if;
        end if;
    end process;
    
    -- ----------------------------------------
    -- # BUSSEN MUX
    -- ----------------------------------------
    with TB select
    buss <= 
        ASR_REG when "111",
        IR_REG when "001",
        ram(conv_integer(ASR_REG(8 downto 0))) when "010",
        PC_REG when "011",
        AR_REG when "100",
        HR_REG when "101",
        current_GR when "110",
        "000" & mram(conv_integer(MPC))(12 downto 0) when "000",
        (others => '0') when others;
    
    with mux1 select
        current_GR <=
            GR0_REG when "00",
            GR1_REG when "01",
            GR2_REG when "10",
            GR3_REG when "11",
        (others => '0') when others;    

    -- ----------------------------------------
    -- # Mikro
    -- ----------------------------------------
    myM <= mram(conv_integer(MPC(5 downto 0)));
    myADR <= myM(6 downto 0);
    SEQ <= myM(10 downto 7);
    LC <= myM(12 downto 11);
    P <= myM(13);
    S <= myM(14);
    FB <= myM(17 downto 15);
    TB <= myM(20 downto 18);
    ALU_OP <= myM(24 downto 21);
    process(CLK) begin
        if rising_edge(CLK) then
            case SEQ is
                when "0000" => MPC <= MPC + 1;
                when "0001" => MPC <= k1(conv_integer(IR_REG(15 downto 12)));
                when "0010" => MPC <= k2(conv_integer('0'&IR_REG(9)));
                when "0011" => MPC <= '0' & myADR;
                when others => MPC <= MPC;
            end case;
            if SEQ = "1000" then
                if flag_newframe = '1' then
                    MPC <= MPC + 1;
                else
                    MPC <= '0' & myADR;
                end if;
            end if;
            if SEQ = "1001" then
                if z_flag = '1' then
                    MPC <= MPC + 1;
                else
                    MPC <= '0' & myADR;
                end if;
            end if;
            
        end if;
    end process;
    -- ----------------------------------------
    -- # ALU
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
            case ALU_OP is
                 when "0000" => null;
                 when "0010" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) and buss(15 downto 0);
                 when "1000" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) + buss(15 downto 0);
                 when "0001" => AR_REG(15 downto 0) <= buss(15 downto 0);
                 when "0011" => AR_REG(15 downto 0) <= X"0000";
                 when "1110" => AR_REG(15 downto 0) <= "0" & AR_REG(15 downto 1);
                 when "1111" => if buss(0) = '1' then z_flag <= '1'; else z_flag <= '0'; end if;
                 when "0101" =>
                            AR_REG(15 downto 0) <= AR_REG(15 downto 0) - buss(15 downto 0);
                            if AR_REG = 0 then
                                z_flag <= '0';
                            else
                                z_flag <= '1';
                            end if;
                 when others => null;
            end case;

            -- ----------------------------------------
            -- # PLAYER MOVER
            -- ----------------------------------------
            if ALU_OP = "1001" then
                -- # Player 1
                if (joystick1(25 downto 24) & joystick1(39 downto 32)) > 450 and (joystick1(25 downto 24) & joystick1(39 downto 32)) < 560 then
                    vel_x1 <= resize(vel_x1 / 2,2,-9); -- # FRICTION
                else
                    jstk_x1 <= (joystick1(25 downto 24) & joystick1(39 downto 32)) xor "1000000000";
                    delta_x1 <= resize(to_sfixed(jstk_x1,0,-9),2,-9);
                    vel_x1 <= resize((vel_x1 + delta_x1),2,-9);
                end if;
                xpos_real1 <= resize(xpos_real1 + vel_x1,9,-4);
                xpos_int1 <= to_integer(xpos_real1);

                if (joystick1(9 downto 8) & joystick1(23 downto 16)) > 450 and (joystick1(9 downto 8) & joystick1(23 downto 16)) < 560 then
                    vel_y1 <= resize(vel_y1 / 2,2,-9); -- # FRICTION
                else
                    jstk_y1 <= (joystick1(9 downto 8) & joystick1(23 downto 16)) xor "1000000000";
                    delta_y1 <= resize(to_sfixed(jstk_y1,0,-9),2,-9);
                    vel_y1 <= resize((vel_y1 + delta_y1),2,-9);
                end if;
                ypos_real1 <= resize(ypos_real1 - vel_y1,9,-4);
                ypos_int1 <= to_integer(ypos_real1);
                -- # Player 2
                if (joystick2(25 downto 24) & joystick2(39 downto 32)) > 450 and (joystick2(25 downto 24) & joystick2(39 downto 32)) < 560 then
                    vel_x2 <= resize(vel_x2 / 2,2,-9);
                else
                    jstk_x2 <= (joystick2(25 downto 24) & joystick2(39 downto 32)) xor "1000000000";
                    delta_x2 <= resize(to_sfixed(jstk_x2,0,-9),2,-9);
                    vel_x2 <= resize(vel_x2 + delta_x2,2,-9);
                end if;
                xpos_real2 <= resize(xpos_real2 + vel_x2,9,-4);
                xpos_int2 <= to_integer(xpos_real2);

                if (joystick2(9 downto 8) & joystick2(23 downto 16)) > 450 and (joystick2(9 downto 8) & joystick2(23 downto 16)) < 560 then
                    vel_y2 <= resize(vel_y2 / 2,2,-9);
                else
                    jstk_y2 <= (joystick2(9 downto 8) & joystick2(23 downto 16)) xor "1000000000";
                    delta_y2 <= resize(to_sfixed(jstk_y2,0,-9),2,-9);
                    vel_y2 <= resize(vel_y2 + delta_y2,2,-9);
                end if;
                ypos_real2 <= resize(ypos_real2 - vel_y2,9,-4);
                ypos_int2 <= to_integer(ypos_real2);
            end if;

            -- ----------------------------------------
            -- # PROJECTILES
            -- ----------------------------------------
            if ALU_OP = "1011" then
                -- # Projectile 1
                if joystick1(1) = '1' then
                    if proj_active1 = '0' then
                        proj_deltax1 <= resize(to_sfixed((not joystick1(25)&joystick1(24)&joystick1(39 downto 32)),3,-6),2,-9);
                        proj_deltay1 <= resize(to_sfixed((not joystick1(9)&joystick1(8)&joystick1(23 downto 16)),3,-6),2,-9);
                        proj_real_xpos1 <= xpos_real1;
                        proj_real_ypos1 <= ypos_real1;
                    end if;
                   proj_active1 <= '1';
                end if;

                if proj_active1 = '1' then
                    proj_real_xpos1 <= resize(proj_real_xpos1 + proj_deltax1,9,-4);
                    proj_xpos1 <= to_integer(proj_real_xpos1);
                    proj_real_ypos1 <= resize(proj_real_ypos1 - proj_deltay1,9,-4);
                    proj_ypos1 <= to_integer(proj_real_ypos1);

                    if to_integer(proj_real_xpos1) < 0 or to_integer(proj_real_xpos1) > 500 or 
                       to_integer(proj_real_ypos1) < 0 or to_integer(proj_real_ypos1) > 479 then
                        proj_active1 <= '0';
                    else 
                        proj_active1 <= '1';
                    end if;

                    if joystick1(2) = '1' then
                        proj_real_xpos1 <= to_sfixed(-1, 9, -4);
                        proj_real_ypos1 <= to_sfixed(-1, 9, -4);
                    end if;
                end if;


                -- # Projectile 2
                if joystick2(1) = '1' then
                    if proj_active2 = '0' then
                        proj_deltax2 <= resize(to_sfixed((not joystick2(25)&joystick2(24)&joystick2(39 downto 32)),3,-6),2,-9);
                        proj_deltay2 <= resize(to_sfixed((not joystick2(9)&joystick2(8)&joystick2(23 downto 16)),3,-6),2,-9);
                        proj_real_xpos2 <= xpos_real2;
                        proj_real_ypos2 <= ypos_real2;
                    end if;
                   proj_active2 <= '1';
                end if;

                if proj_active2 = '1' then
                    proj_real_xpos2 <= resize(proj_real_xpos2 + proj_deltax2,9,-4);
                    proj_xpos2 <= to_integer(proj_real_xpos2);
                    proj_real_ypos2 <= resize(proj_real_ypos2 - proj_deltay2,9,-4);
                    proj_ypos2 <= to_integer(proj_real_ypos2);

                    if to_integer(proj_real_xpos2) < 0 or to_integer(proj_real_xpos2) > 550 or 
                       to_integer(proj_real_ypos2) < 0 or to_integer(proj_real_ypos2) > 479 then
                        proj_active2 <= '0';
                    else 
                        proj_active2 <= '1';
                    end if;
                end if;

                if joystick2(2) = '1' then
                    proj_real_xpos2 <= to_sfixed(-1, 9, -4);
                    proj_real_ypos2 <= to_sfixed(-1, 9, -4);
                end if;
            end if;

            if ALU_OP = "1100" then
                if to_integer(xpos_real1) > to_integer(proj_real_xpos2) - 16 and to_integer(xpos_real1) < to_integer(proj_real_xpos2) + 16 and 
                   to_integer(ypos_real1) > to_integer(proj_real_ypos2) - 16 and to_integer(ypos_real1) < to_integer(proj_real_ypos2) + 16 then 
                    -- Remove proj
                    proj_real_xpos2 <= to_sfixed(-1, 9, -4);
                    proj_real_ypos2 <= to_sfixed(-1, 9, -4);
                    -- player1 hit
                    hit_counter1 <= 15;
                end if;
                if to_integer(xpos_real2) > to_integer(proj_real_xpos1) - 16 and to_integer(xpos_real2) < to_integer(proj_real_xpos1) + 16 and 
                   to_integer(ypos_real2) > to_integer(proj_real_ypos1) - 16 and to_integer(ypos_real2) < to_integer(proj_real_ypos1) + 16 then 
                    -- Remove proj
                    proj_real_xpos1 <= to_sfixed(-1, 9, -4);
                    proj_real_ypos1 <= to_sfixed(-1, 9, -4);
                    -- player2 hit
                    hit_counter2 <= 15;
                end if;             

                if hit_counter1 > 0 then
                    vel_x1 <= proj_deltax2;
                    vel_y1 <= proj_deltay2;
                    hit_counter1 <= hit_counter1 - 1;
                end if; 
                if hit_counter2 > 0 then
                    vel_x2 <= proj_deltax1;
                    vel_y2 <= proj_deltay1;
                    hit_counter2 <= hit_counter2 - 1;
                end if;  
            end if;

            if ALU_OP = "1101" then
                AR_REG(15 downto 0) <= X"0000"; 
                if horse_tile1 = "010" then --lava
                    AR_REG(0) <= '1';
                end if;
                if horse_tile2 = "010" then --lava
                    AR_REG(1) <= '1';
                end if;


                --if map_counter = X"FF" then      
                --    cur_map <= cur_map + 1;
                --end if;   
                --map_counter <= map_counter + 1;


            end if;

        end if;
    end process;

end rtl;
