
library IEEE;
use IEEE.STD_LOGIC_1164.all;

package design_pkg is
    constant UROL : INTEGER RANGE 0 TO 4 := 4; 

    --! Reverse the Byte order of the input word.
    function reverse_byte(
        vec : std_logic_vector
    ) return std_logic_vector;
   
    
    --! Reverse the Bit order of the input vector.
    function reverse_bit(
        vec : std_logic_vector
    ) return std_logic_vector;
 

    function trunc_data(
        datain_tvalid : std_logic;
        datain_tlast : std_logic;
        data_in_tdata : std_logic_vector; 
        data_in_tkeep : std_logic_vector
    ) return std_logic_vector;     
 
    
    --! Padding the current word.
    function pad_data_in(
        datain_tvalid : std_logic;
        datain_tlast : std_logic;
        data_in_tdata : std_logic_vector; 
        data_in_tkeep : std_logic_vector
    ) return std_logic_vector;    
    
    -- State signals
    TYPE state_t IS (
        IDLE,
        LOAD_KEY,
        LOAD_NONCE,
        INIT_SETUP,
        INIT,
        INIT_KEY_ADD,
        ABSORB_AD,
        PROCESS_AD,
        PROCESS_LAST_AD,
        EMPTY_AD,
        ABSORB_MSG,
        PROCESS_MSG,
        PROCESS_LAST_MSG,
        EMPTY_MSG,
        FINAL_KEY_ADD_1,
        FINAL_PROCESS,
        FINAL_KEY_ADD_2
        
    );


end design_pkg;


package body design_pkg is


    --! Reverse the Byte order of the input word.
    function reverse_byte( vec : std_logic_vector ) return std_logic_vector is
        variable res : std_logic_vector(vec'length - 1 downto 0);
        constant n_bytes  : integer := vec'length/8;
    begin

        -- Check that vector length is actually byte aligned.
        assert (vec'length mod 8 = 0)
            report "Vector size must be in multiple of Bytes!" severity failure;

        -- Loop over every byte of vec and reorder it in res.
        for i in 0 to (n_bytes - 1) loop
            res(8*(i+1) - 1 downto 8*i) := vec(8*(n_bytes - i) - 1 downto 8*(n_bytes - i - 1));
        end loop;

        return res;
    end function reverse_byte;

    --! Reverse the Bit order of the input vector.
    function reverse_bit( vec : std_logic_vector ) return std_logic_vector is
        variable res : std_logic_vector(vec'length - 1 downto 0);
    begin
        for i in 0 to (vec'length - 1) loop
            res(i) := vec(vec'length - i - 1);
        end loop;
        return res;
    end function reverse_bit;

    --! Padd the data with 0x80 Byte if pad_loc is set.
    function trunc_data( datain_tvalid : std_logic; datain_tlast : std_logic; data_in_tdata : std_logic_vector; data_in_tkeep : std_logic_vector) return std_logic_vector is
        variable res : std_logic_vector(data_in_tdata'length - 1 downto 0) := data_in_tdata;
    begin
        if (datain_tvalid = '1' AND datain_tlast = '1') then
            for i in 0 to (data_in_tkeep'length - 1) loop
                if (data_in_tkeep(i) = '0') then
                    res(8*(i+1) - 1 downto 8*i) := (OTHERS => '0');
                end if;
            end loop;
        end if;
        return res;
    end function;


    --! Padd the data with 0x80 Byte if pad_loc is set.
    function pad_data_in( datain_tvalid : std_logic; datain_tlast : std_logic; data_in_tdata : std_logic_vector; data_in_tkeep : std_logic_vector) return std_logic_vector is
        variable res : std_logic_vector(data_in_tdata'length - 1 downto 0) := data_in_tdata;
    begin
        if (datain_tvalid = '1' AND datain_tlast = '1') then
            for i in 0 to (data_in_tkeep'length - 1) loop
                if (data_in_tkeep(i) = '0') then
                    res(8*(i+1) - 1 downto 8*i) := res(8*(i+1) - 1 downto 8*i) XOR x"80";
                    exit;
                end if;
            end loop;
        end if;
        return res;
    end function;

end package body design_pkg;
