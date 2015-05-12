library IEEE;
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
	outPos2 : out std_logic_vector (19 downto 0)
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
    signal GR1_REG : STD_LOGIC_VECTOR(15 downto 0) := X"00F0"; -- Pos X P1
    signal GR2_REG : STD_LOGIC_VECTOR(15 downto 0) := X"00F0"; -- Pos Y P1
    signal GR3_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    
    -- PM/RAM och MyM
    type ram_type is array (0 to 15) of std_logic_vector(15 downto 0);
    type mram_type is array (0 to 29) of std_logic_vector(24 downto 0);

    signal ram : ram_type := (
    -- Programkod
    X"5400", X"6800", X"7000", X"8000",
    X"0000", X"0000", X"0000", X"0000", 
    X"0000", X"0000", X"0000", X"0000", 
    X"0000", X"0000", X"0000", X"0000");

    constant mram : mram_type := (
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "011" & "111" & "0" & "00" & "0" & "0000" & "0000000", --0x00 -- Ladda nästa PM
    "0000" & "010" & "001" & "0" & "00" & "0" & "0000" & "0000000", --0x01
    "0000" & "000" & "000" & "0" & "00" & "0" & "0010" & "0000000", --0x02
    "0000" & "001" & "111" & "0" & "00" & "0" & "0001" & "0000000", --0x03
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x04
    "0000" & "010" & "110" & "0" & "01" & "0" & "0011" & "0000000", --0x05 - Load
    "0000" & "110" & "010" & "0" & "01" & "0" & "0011" & "0000000", --0x06 - Store
    "0001" & "110" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x07 - Add
    "0100" & "010" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x08
    "0000" & "100" & "110" & "0" & "01" & "0" & "0011" & "0000000", --0x09
    "0001" & "110" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x0A - Sub
    "0101" & "010" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x0B
    "0000" & "100" & "110" & "0" & "01" & "0" & "0011" & "0000000", --0x0C
    "0001" & "110" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x0D - Handle X
    "1110" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x0E 
    "0000" & "100" & "110" & "0" & "01" & "0" & "0011" & "0000000", --0x0F
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0001" & "110" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x20 - Handle Y
    "1111" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x21
    "0000" & "100" & "110" & "0" & "01" & "0" & "0011" & "0000000", --0x22
    "0110" & "000" & "000" & "0" & "01" & "0" & "0011" & "0000000", --0x23 - Update horse
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x24
    "0000" & "001" & "111" & "0" & "11" & "0" & "0011" & "0000000", --0x25 -- JMP
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x26
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x27
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x28
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x29
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x2A
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x2B
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000", --0x0E
    "0000" & "000" & "000" & "0" & "00" & "0" & "0000" & "0000000"  --0x0F
    );
    
    -- K1 och K2
    type k1_type is array(0 to 15) of std_logic_vector(7 downto 0);
    type k2_type is array(0 to 3) of std_logic_vector(7 downto 0);

    constant k1 : k1_type := (
    X"00", -- NOP
    X"05", -- Load	1
    X"06", -- Store	2
    X"07", -- Add	3
    X"0A", -- Sub	4
    X"0D", -- Handle X	5
    X"20", -- Handle Y	6
    X"23", -- Horse	7
    X"25", -- JMP   8
    X"00",
    X"00",
    X"00",
    X"00",
    X"00",
    X"00",
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
    signal P : std_logic_vector(1 downto 0);
    signal LC : std_logic;
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

    signal delta : real := 0.1;
    
begin
    --mem<=ram(to_integer(unsigned(ind)));
    --b1<= joystick1(1);
 



    process(NEW_FRAME) begin
        if rising_edge(NEW_FRAME) then

            --if(joystick1(25 downto 24) & joystick1(39 downto 32) > 600) then
            --    xpos1 <= xpos1 + 1;
            --elsif(joystick1(25 downto 24) & joystick1(39 downto 32) < 300) then
            --    xpos1 <= xpos1 - 1;
            --end if;

            --if(joystick1(9 downto 8) & joystick1(23 downto 16) > 600) then
            --    ypos1 <= ypos1 - 1;
            --elsif(joystick1(9 downto 8) & joystick1(23 downto 16) < 300) then
            --    ypos1 <= ypos1 + 1;
            --end if;
            --outPos1 <= xpos1 & ypos1;

            --if(joystick2(25 downto 24) & joystick2(39 downto 32) > 600) then
            --    xpos2 <= xpos2 + 1;
            --elsif(joystick2(25 downto 24) & joystick2(39 downto 32) < 300) then
            --    xpos2 <= xpos2 - 1;
            --end if;

            --if(joystick2(9 downto 8) & joystick2(23 downto 16) > 600) then
            --    ypos2 <= ypos2 - 1;
            --elsif(joystick2(9 downto 8) & joystick2(23 downto 16) < 300) then
            --    ypos2 <= ypos2 + 1;
            --end if;

            --outPos1 <= xpos1 & ypos1;
            --outPos2 <= xpos2 & ypos2;

        end if;
    end process;

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
                ram((ASR_REG(7 downto 0))) <= buss(15 downto 0);
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
            elsif (P = "01") then
                PC_REG <= PC_REG + 1;
            elsif (P="11") then
                PC_REG <= ASR_REG;
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
        ram(conv_integer(ASR_REG(7 downto 0))) when "010",
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
    LC <= myM(11);
    P <= myM(13 downto 12);
    S <= myM(14);
    FB <= myM(17 downto 15);
    TB <= myM(20 downto 18);
    ALU_OP <= myM(24 downto 21);
    process(CLK) begin
        if rising_edge(CLK) then
            case SEQ is
                when "0000" => MPC <= MPC + 1;
                when "0001" => MPC <= k1(conv_integer(IR_REG(15 downto 12)));
                when "0010" => MPC <= k2(conv_integer(IR_REG(9 downto 8)));
                when "0011" => MPC <= '0' & myADR;
                when others => MPC <= MPC;
            end case;
        end if;
    end process;
    -- ----------------------------------------
    -- # ALU
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
  --          case ALU_OP is
  --              when "0000" => null;
  --              when "0100" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) + buss(15 downto 0); -- ADD
  --              when "0101" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) - buss(15 downto 0); -- SUB
  --              when "0001" => AR_REG(15 downto 0) <= buss(15 downto 0); -- LOAD
		--when "0011" => AR_REG(15 downto 0) <= X"0000"; -- RESET
		--when "1110" => -- handle X
		--	x <=joystick1(25 downto 24) & joystick1(39 downto 32);
		--	if(x > 600) then
		--		AR_REG(15 downto 0) <= AR_REG(15 downto 0) + 1;
		--	elsif(x < 300) then
		--		AR_REG(15 downto 0) <= AR_REG(15 downto 0) - 1;
		--	end if;
		--when "1111" => -- handle Y
		--	y<=joystick1(9 downto 8) & joystick1(23 downto 16);
		--	if(y > 600) then
		--		AR_REG(15 downto 0) <= AR_REG(15 downto 0) - 1;
		--	elsif(y < 300) then
		--		AR_REG(15 downto 0) <= AR_REG(15 downto 0) + 1;
		--	end if;
		--when "0110" => --outPos1 <= GR1_REG(9 downto 0) & GR2_REG (9 downto 0);
  --              when others => null;
  --          end case;
        end if;
    end process;

end rtl;
