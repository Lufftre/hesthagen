library IEEE;
    use IEEE.std_logic_1164.all;
    use IEEE.std_logic_arith.all;
    use IEEE.std_logic_unsigned.all;
    
entity bussen is
    port (
        CLK: in std_logic;

        RST: in std_logic;

	btnu : in std_logic;

       	MISO_1 : in  STD_LOGIC;								-- 
        SS_1 : out  STD_LOGIC;								-- Slave 
        MOSI_1 : out  STD_LOGIC;							-- Master Out Slave In, 
        SCLK_1 : out  STD_LOGIC;
       	MISO_2 : in  STD_LOGIC;								-- 
        SS_2 : out  STD_LOGIC;								-- Slave 
        MOSI_2 : out  STD_LOGIC;							-- Master Out Slave In, 
        SCLK_2 : out  STD_LOGIC;

	Hsync,Vsync : out  STD_LOGIC;
        vga: out  STD_LOGIC_VECTOR (7 downto 0); -- ?? 
							-- Serial Clock, Pin 4, 
	AN : out  STD_LOGIC_VECTOR (3 downto 0);  -- Anodes for Seven Segment Display
           
        SEG : out  STD_LOGIC_VECTOR (6 downto 0) -- Cathodes for Seven Segment Display
    );
end entity;

architecture rtl of bussen is


    -- ----------------------------------------
    -- # COMPONENTS
    -- ----------------------------------------

	component ClkDiv_5Hz
	 Port ( CLK : in  STD_LOGIC;
	  RST : in  STD_LOGIC;
	  CLKOUT : inout STD_LOGIC
	);
	end component;	

	component PmodJSTK
	  Port ( 
	  CLK : in  STD_LOGIC;
	  RST : in  STD_LOGIC;
	  sndRec : in  STD_LOGIC;
	  DIN : in  STD_LOGIC_VECTOR (7 downto 0);
	  MISO : in  STD_LOGIC;
	  SS : out  STD_LOGIC;
	  SCLK : out  STD_LOGIC;
	  MOSI : out  STD_LOGIC;
	  DOUT : inout  STD_LOGIC_VECTOR (39 downto 0)
	);
	end component;

	component GPU
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
	    ypos_int2 : in integer range 0 to 479
	    );
	end component;

	component CPU 
    	Port (
        CLK : in  STD_LOGIC;
	NEW_FRAME : in std_logic;
	RST : in  STD_LOGIC;
	btnu : in std_logic;
	joystick1, joystick2 : in  STD_LOGIC_VECTOR (39 downto 0);
	mem : out std_logic_vector(15 downto 0);
	outPos1, outPos2 : out std_logic_vector (19 downto 0);
    xpos_int1 : out integer range 0 to 639;
    ypos_int1 : out integer range 0 to 479;
    xpos_int2 : out integer range 0 to 639;
    ypos_int2 : out integer range 0 to 479
	
);
	end component;

	component ssdCtrl

    	Port ( CLK : in  STD_LOGIC;
           RST : in  STD_LOGIC;
           DIN : in  STD_LOGIC_VECTOR (9 downto 0);
           AN : out  STD_LOGIC_VECTOR (3 downto 0);
           SEG : out  STD_LOGIC_VECTOR (6 downto 0));
	end component;
    -- ----------------------------------------
    -- # SIGNALS
    -- ----------------------------------------

   signal m : std_logic_vector(15 downto 0) := X"0000";


   -- JOYSTICK 1
   signal joystick1 : STD_LOGIC_VECTOR (39 downto 0);
   signal joystick2 : STD_LOGIC_VECTOR (39 downto 0);
	-- Holds data to be sent to PmodJSTK
   signal sndData : STD_LOGIC_VECTOR(7 downto 0) := X"83";
	-- Signal to send/receive data to/from PmodJSTK
   signal sndRec : STD_LOGIC;

   signal pos_P1 : std_logic_vector (19 downto 0);
   signal pos_P2 : std_logic_vector (19 downto 0);
   signal posProj1, posProj2 : std_logic_vector (19 downto 0);

   signal frame_pulse : std_logic;

   signal xpos_int1 : integer range 0 to 639;
   signal ypos_int1 : integer range 0 to 479;
   signal xpos_int2 : integer range 0 to 639;
   signal ypos_int2 : integer range 0 to 479;

    
begin


genSndRec : ClkDiv_5Hz port map(
	CLK=>CLK,
	RST=>RST,
	CLKOUT=>sndRec
);

gpu1 : GPU port map(
	CLK=>CLK,
	RST=>RST,
	posP1=>pos_P1,
	posP2=>pos_P2,
	posProj1=>posProj1,
	posProj2=>posProj2,
	Hsync=>Hsync,
	Vsync=>Vsync,
	vga=>vga,
	NEW_FRAME=>frame_pulse,
	xpos_int1=>xpos_int1,
	ypos_int1=>ypos_int1,
	xpos_int2=>xpos_int2,
	ypos_int2=>ypos_int2
);
	
  JSTK1 : PmodJSTK port map(
	CLK=>CLK,
	RST=>RST,
	sndRec=>sndRec,
	DIN=>sndData,
	MISO=>MISO_1,
	SS=>SS_1,
	SCLK=>SCLK_1,
	MOSI=>MOSI_1,
	DOUT=>joystick1
);

  JSTK2 : PmodJSTK port map(
	CLK=>CLK,
	RST=>RST,
	sndRec=>sndRec,
	DIN=>sndData,
	MISO=>MISO_2,
	SS=>SS_2,
	SCLK=>SCLK_2,
	MOSI=>MOSI_2,
	DOUT=>joystick2
);

   DispCtrl : ssdCtrl port map(
	  CLK=>CLK,
	  RST=>RST,
	  DIN=>m(9 downto 0),
	  AN=>AN,
	  SEG=>SEG
);

	cpu1: CPU port map(
    CLK=>CLK,
    RST=>RST,
	NEW_FRAME=>frame_pulse,
	btnu=>btnu,
	joystick1=>joystick1,
	joystick2=>joystick2,
	mem=>m,
	outPos1=>pos_P1,
	outPos2=>pos_P2,
	xpos_int1=>xpos_int1,
	ypos_int1=>ypos_int1,
	xpos_int2=>xpos_int2,
	ypos_int2=>ypos_int2
    ); 

end architecture;
