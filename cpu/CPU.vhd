library IEEE;
library ieee_proposed;
  use ieee_proposed.fixed_pkg.all;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity CPU is
    Port ( 
    CLK : in  STD_LOGIC;
    NEW_FRAME : in std_logic;
    RST : in  STD_LOGIC;
    btnu : in std_logic;
    joystick1 : in  STD_LOGIC_VECTOR (39 downto 0);
    joystick2 : in  STD_LOGIC_VECTOR (39 downto 0);
    mem : out std_logic_vector(15 downto 0);
    outPos1 : out std_logic_vector(19 downto 0);
    outPos2 : out std_logic_vector (19 downto 0);
    xpos_int1 : out integer range 0 to 639;
    ypos_int1 : out integer range 0 to 479;
    xpos_int2 : out integer range 0 to 639;
    ypos_int2 : out integer range 0 to 479;
    proj_xpos1 : out integer range 0 to 639;
    proj_ypos1 : out integer range 0 to 479;
    proj_xpos2 : out integer range 0 to 639;
    proj_ypos2 : out integer range 0 to 479
    );
end entity;
architecture rtl of CPU is
    -- Register
    signal ASR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal PC_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal AR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal HR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal IR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal GR0_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; 
    signal GR1_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; 
    signal GR2_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; 
    signal GR3_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal flag_newframe : STD_LOGIC := '0';
    
    -- PM/RAM och MyM
    type ram_type is array (0 to 31) of std_logic_vector(15 downto 0);
    type mram_type is array (0 to 45) of std_logic_vector(24 downto 0);

    --signal ram : ram_type := (
    ---- Programkod
    --X"B018", X"141E", X"241C", X"141F",
    --X"241D", X"7010", X"0000", X"0000", 
    --X"0000", X"0000", X"0000", X"0000", 
    --X"0000", X"0000", X"0000", X"0000",
    --X"0000", X"0000", X"0000", X"0000",
    --X"0000", X"0000", X"0000", X"0000", 
    --X"0000", X"0000", X"00F0", X"00F0", 
    --X"00FF", X"00FF", X"0000", X"0000"
    --);

    signal ram : ram_type := (
    -- Programkod
    X"B00C", -- 0x00 B??
    X"C000",--  0x01 LOAD VEL-X gr0
    X"D000", -- 0x02 ADD gr0 JSTK-X
    X"E000", -- 0x03 STORE gr0 VELX
    X"700C",--  0x04 ADD gr0 POSX1
    X"0000", -- 0x05 STORE gr0 POSX1
    X"0000",--  0x01 LOAD VEL-Y gr0
    X"0000", -- 0x02 ADD gr0 JSTK-Y
    X"0000", -- 0x03 STORE gr0 VELX
    X"0000",--  0x04 ADD gr0 POSX1
    X"0000", -- 0x05 STORE gr0 POSX1
    X"0000",--  0x0B JMP 0
    X"0000", -- 0x0C 
    X"0000", -- 0x0D 
    X"0000", -- 0x0E 
    X"0000", -- 0x0F 
    X"0000",--  0x10 posx Player1
    X"0000", -- 0x11 posy
    X"0000", -- 0x12 velx
    X"0000",--  0x13 vely
    X"0000", -- 0x14 deltax
    X"0000",  --0x15 deltay
    X"0000",--  0x16 posx Player2
    X"0000", -- 0x17 posy
    X"0000", -- 0x18 velx
    X"0000",--  0x19 vely
    X"0000", -- 0x1A deltax
    X"0000",  --0x1B deltay
    X"0000", -- 0x1C 
    X"0000", -- 0x1D 
    X"0000", -- 0x1E JSTK X
    X"0000" --  0x1F JSTK Y
    );



    constant mram : mram_type := (
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "011" & "111" & "0" & "0" & "00" & "0000" & "0000000", --0x00 PC => ASR, mpc++,        (HÄMTFAS)
    "0000" & "010" & "001" & "0" & "0" & "00" & "0000" & "0000000", --0x01 PM => IR, mpc++ 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0010" & "0000000", --0x02 K1 => mpc
    "0000" & "001" & "111" & "0" & "0" & "00" & "0001" & "0000000", --0x03 IR => ASR, K2 => mpc
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x04
    "0000" & "010" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x05 PM => GRx, mpc = 0,      (LOAD)
    "0000" & "110" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x06 GRx => PM, mpc = 0,      (STORE)
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x07 Grx => AR, mpc++,        (ADD)
    "1000" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x08 (AR + PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x09 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0A Grx => AR, mpc++,        (SUB)
    "0101" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0B (AR - PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x0C AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0D GRx => AR, mpc++         (x-move)
    "1110" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0E ALU magic, mpc++ 
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x0F AR => GRx, PC++, mpc = 0
--    ALU     TB      FB      S     P    LC      SEQ       myADR      
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x10 GRx => AR, mpc++         (y-move)
    "1111" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x11 ALU magic, mp++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x12 AR => GRx, PC++, mpc = 0
    "0000" & "010" & "011" & "0" & "0" & "00" & "0011" & "0000000", --0x13 PM => PC, mpc = 0        (JMP)
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x14 Grx => AR, mpc++,        (LSR)
    "1110" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x15 (AR LSR 1) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x16 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x17 Grx => AR, mpc++,        (LSR)
    "1111" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x18 (NOT AR(10) ) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x19 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x20 GRx => AR, mpc++         (AND)
    "0010" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x21 ALU magic, mp++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x22 AR => GRx, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0011" & "0000000", --0x1D mpc = 0                  (NOP)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1000" & "0010011", --0x1E IF flagga = 1 then mpc++ else mpc =>myADR (B??)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1F PC++, mpc = 0
--    ALU     TB      FB      S     P    LC      SEQ       myADR      
    "1001" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x07 Grx => AR, mpc++,        (SUPER ADD)
    "1011" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x07 Grx => AR, mpc++,        (SUPER PEWPEW)
    "1100" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x07 Grx => AR, mpc++,        (SUPER HIT)
    "0000" & "010" & "011" & "0" & "0" & "00" & "0011" & "0000000", --0x23 
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x24 
    "1110" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x25 
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x26 
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x27 
    "1111" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x28 
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x29 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2A
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2B
    "0000" & "000" & "000" & "0" & "0" & "00" & "0011" & "0000000", --0x2E 
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
    X"0D", -- x-move 0x05
    X"10", -- y-move 0x06
    X"13", -- JMP    0x07
    X"24", -- LSR    0x08
    X"27", -- NOT(10)0x09
    X"20", -- AND    0x0A
    X"1E", -- B??    0x0B
    X"20", -- SADD   0x0C
    X"21", -- proj   0x0D
    X"22", -- colli  0x0E
    X"00"
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

    alias xpos : std_logic_vector (9 downto 0) is GR1_REG(9 downto 0);
    alias ypos : std_logic_vector (9 downto 0) is GR2_REG(9 downto 0);
    signal b1 : std_logic;
    --signal b2 : std_logic := joystick1(2);

    signal x : std_logic_vector (9 downto 0);
    signal y : std_logic_vector (9 downto 0);

    signal xpos1 : std_logic_vector (9 downto 0) := "00" & X"F0";
    signal ypos1 : std_logic_vector (9 downto 0) := "00" & X"F0";
    signal xpos2 : std_logic_vector (9 downto 0) := "00" & X"F0";
    signal ypos2 : std_logic_vector (9 downto 0) := "00" & X"F0";


    signal ind : std_logic_vector (3 downto 0);

    signal lastvalue : std_logic := '0';

    signal isNeg : std_logic := '0';


    signal vel_x1 : sfixed(4 downto -9) := to_sfixed(0, 4, -9);
    signal delta_x1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal xpos_real1 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_x1 : std_logic_vector(9 downto 0);

    signal vel_y1 : sfixed(4 downto -9) := to_sfixed(0, 4, -9);
    signal delta_y1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal ypos_real1 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_y1 : std_logic_vector(9 downto 0);

    signal vel_x2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal delta_x2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal xpos_real2 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_x2 : std_logic_vector(9 downto 0);

    signal vel_y2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal delta_y2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal ypos_real2 : sfixed(9 downto -4) := to_sfixed(320, 9, -4);
    signal jstk_y2 : std_logic_vector(9 downto 0);

    signal proj_dirx1 : std_logic_vector(9 downto 0);
    signal proj_diry1 : std_logic_vector(9 downto 0);
    signal proj_deltax1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_deltay1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_active1 : std_logic := '0';
    signal proj_real_xpos1 : sfixed(9 downto -4) := to_sfixed(120, 9, -4);
    signal proj_real_ypos1 : sfixed(9 downto -4) := to_sfixed(120, 9, -4);

    signal proj_dirx2 : std_logic_vector(9 downto 0);
    signal proj_diry2 : std_logic_vector(9 downto 0);
    signal proj_deltax2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_deltay2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal zero_vel : sfixed(2 downto -9) := to_sfixed(0, 2, -9);
    signal proj_active2 : std_logic := '0';
    signal proj_real_xpos2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4);
    signal proj_real_ypos2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4);

begin
    --mem <= ram(24);

    flag_newframe <= NEW_FRAME;


    --process(CLK) begin
    --    if rising_edge(CLK) then
    --        if flag_newframe = '1' then
    --            if (joystick1(25 downto 24) & joystick1(39 downto 32)) > 450 and (joystick1(25 downto 24) & joystick1(39 downto 32)) < 560 then
    --                vel_x <= resize(vel_x / 2,2,-9);
    --            else
    --                jstk_x <= (joystick1(25 downto 24) & joystick1(39 downto 32)) xor "1000000000";
    --                delta_x <= resize(to_sfixed(jstk_x,0,-9),2,-9);
    --                vel_x <= resize(vel_x + delta_x,2,-9);
    --            end if;
    --            xpos_real <= resize(xpos_real + vel_x,9,-4);
    --            xpos_int <= to_integer(xpos_real);

    --            if (joystick1(9 downto 8) & joystick1(23 downto 16)) > 450 and (joystick1(9 downto 8) & joystick1(23 downto 16)) < 560 then
    --                vel_y <= resize(vel_y / 2,2,-9);
    --            else
    --                jstk_y <= (joystick1(9 downto 8) & joystick1(23 downto 16)) xor "1000000000";
    --                delta_y <= resize(to_sfixed(jstk_y,0,-9),2,-9);
    --                vel_y <= resize(vel_y + delta_y,2,-9);
    --            end if;
    --            ypos_real <= resize(ypos_real - vel_y,9,-4);
    --            ypos_int <= to_integer(ypos_real);
    --        end if;
    --    end if;
    --end process;



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
            if flag_newframe = '1' then
                ram(30) <= "0000000000000" & joystick1(25) & joystick1(24) & joystick1(39);
                ram(31) <= "0000000000000" & joystick1(9) & joystick1(8) & joystick1(23);
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

                 when "0101" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) - buss(15 downto 0);
                 when "0001" => AR_REG(15 downto 0) <= buss(15 downto 0);
                 when "0011" => AR_REG(15 downto 0) <= X"0000";
                 when "1110" => AR_REG(15 downto 0) <= "00" & AR_REG(15 downto 2);
                 when "1111" => AR_REG(15 downto 0) <=  AR_REG(15 downto 0) xor X"0800";
                 when others => null;
            end case;

            -- ----------------------------------------
            -- # PLAYER MOVER
            -- ----------------------------------------
            if ALU_OP = "1001" then
                -- # Player 1
                vel_x1 <= resize(vel_x1 - 0.1,4,-9); -- # FRICTION
                vel_y1 <= resize(vel_y1 - 0.1,4,-9); -- # FRICTION
                if (joystick1(25 downto 24) & joystick1(39 downto 32)) > 450 and (joystick1(25 downto 24) & joystick1(39 downto 32)) < 560 then
                    --
                else
                    jstk_x1 <= (joystick1(25 downto 24) & joystick1(39 downto 32)) xor "1000000000";
                    delta_x1 <= resize(to_sfixed(jstk_x1,0,-9),2,-9);
                    vel_x1 <= resize(vel_x1 + delta_x1,4,-9);
                end if;
                xpos_real1 <= resize(xpos_real1 + vel_x1,9,-4);
                xpos_int1 <= to_integer(xpos_real1);

                if (joystick1(9 downto 8) & joystick1(23 downto 16)) > 450 and (joystick1(9 downto 8) & joystick1(23 downto 16)) < 560 then
                    --
                else
                    jstk_y1 <= (joystick1(9 downto 8) & joystick1(23 downto 16)) xor "1000000000";
                    delta_y1 <= resize(to_sfixed(jstk_y1,0,-9),2,-9);
                    vel_y1 <= resize(vel_y1 + delta_y1,4,-9);
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

                    if to_integer(proj_real_xpos1) < 0 or to_integer(proj_real_xpos1) > 620 or to_integer(proj_real_ypos1) < 0 or to_integer(proj_real_ypos1) > 479 then
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

                    if to_integer(proj_real_xpos2) < 0 or to_integer(proj_real_xpos2) > 620 or to_integer(proj_real_ypos2) < 0 or to_integer(proj_real_ypos2) > 479 then
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
                    -- player hit
                    vel_x1 <= resize(proj_deltax2*8,4,-9);
                    vel_y1 <= resize(proj_deltay2*8,4,-9);
                end if;
                if to_integer(xpos_real2) > to_integer(proj_real_xpos1) - 16 and to_integer(xpos_real2) < to_integer(proj_real_xpos1) + 16 and 
                   to_integer(ypos_real2) > to_integer(proj_real_ypos1) - 16 and to_integer(ypos_real2) < to_integer(proj_real_ypos1) + 16 then 
                    -- Remove proj
                    proj_real_xpos1 <= to_sfixed(-1, 9, -4);
                    proj_real_ypos1 <= to_sfixed(-1, 9, -4);
                    -- player hit
                    vel_x2 <= proj_deltax1;
                    vel_y2 <= proj_deltay1;
                end if;                
            end if;


        end if;
    end process;

end rtl;
