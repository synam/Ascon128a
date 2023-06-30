--------------------------------------------------------------------------------
--! @file       ascon_128a.vhd
--! @brief      Implementation of Ascon-128a
--!
--! @author     Tran Sy Nam <transynam1989@gmail.com>
--! @license    This project is released under the GNU Public License.          
--!             The license and distribution terms for this file may be         
--!             found in the file LICENSE in this distribution or at            
--!             http://www.gnu.org/licenses/gpl-3.0.txt                                                                    
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;
USE ieee.std_logic_misc.ALL;
USE work.design_pkg.ALL;

entity ascon_128a is
    port (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;
        en_dec : IN STD_LOGIC;
        key : IN STD_LOGIC_VECTOR (127 DOWNTO 0);
        key_valid : IN STD_LOGIC;
        nonce : IN STD_LOGIC_VECTOR (127 DOWNTO 0);
        nonce_valid : IN STD_LOGIC;
        associated_data_tdata : IN STD_LOGIC_VECTOR (127 DOWNTO 0);
        associated_data_tkeep : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
        associated_data_tvalid : IN STD_LOGIC;
        associated_data_tlast : IN STD_LOGIC;
        datain_tdata : IN STD_LOGIC_VECTOR (127 DOWNTO 0);
        datain_tkeep : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
        datain_tvalid : IN STD_LOGIC;
        datain_tlast : IN STD_LOGIC;
        dataout_tdata : OUT STD_LOGIC_VECTOR (127 DOWNTO 0);
        dataout_tkeep : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
        dataout_tvalid : OUT STD_LOGIC;
        dataout_tlast : OUT STD_LOGIC;
        key_init_done : OUT STD_LOGIC;
        tag : OUT STD_LOGIC_VECTOR (127 DOWNTO 0);   
        tag_valid : OUT STD_LOGIC
    );
end ascon_128a;

architecture Behavioral of ascon_128a is

        COMPONENT Asconp
            PORT (
                        state_in : IN STD_LOGIC_VECTOR(319 DOWNTO 0);
                        rcon : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
                        state_out : OUT STD_LOGIC_VECTOR(319 DOWNTO 0)
                  );
        END COMPONENT;



   CONSTANT IV_AEAD : std_logic_vector(63 DOWNTO 0) := X"80800c0800000000";

   SIGNAL next_state, state : state_t;
   SIGNAL ascon_state, ascon_state_next: std_logic_vector(319 DOWNTO 0);
   SIGNAL ascon_out : STD_LOGIC_VECTOR(319 DOWNTO 0);
   SIGNAL rcon, rcon_next : STD_LOGIC_VECTOR(3 DOWNTO 0);
   SIGNAL ascon_out1 : STD_LOGIC_VECTOR(319 DOWNTO 0);
   SIGNAL rcon1, rcon_next1 : STD_LOGIC_VECTOR(3 DOWNTO 0);   
   SIGNAL key_tmp : STD_LOGIC_VECTOR(127 DOWNTO 0);
   SIGNAL ascon_reg : STD_LOGIC_VECTOR(319 DOWNTO 0);
   SIGNAL data_padd : STD_LOGIC_VECTOR(127 DOWNTO 0);
   
   SIGNAL ascon_fast_state: std_logic_vector(319 DOWNTO 0);
   SIGNAL ascon_fast_out : STD_LOGIC_VECTOR(319 DOWNTO 0);
   SIGNAL rcon_fast, rcon_fast_next : STD_LOGIC_VECTOR(3 DOWNTO 0);   

   SIGNAL dataout_tdata_s : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL dataout_tkeep_s : STD_LOGIC_VECTOR (15 DOWNTO 0);
   SIGNAL dataout_tvalid_s : STD_LOGIC;
   SIGNAL dataout_tlast_s : STD_LOGIC;   
   
   SIGNAL datain_tdata_s : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL datain_tkeep_s : STD_LOGIC_VECTOR (15 DOWNTO 0);
   SIGNAL datain_tvalid_s : STD_LOGIC;
   SIGNAL datain_tlast_s : STD_LOGIC;   
      
   SIGNAL associated_data_tdata_s : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL associated_data_tkeep_s : STD_LOGIC_VECTOR (15 DOWNTO 0);
   SIGNAL associated_data_tvalid_s : STD_LOGIC;
   SIGNAL associated_data_tlast_s : STD_LOGIC;   
         

   SIGNAL data_tmp : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL tag_s : STD_LOGIC_VECTOR (127 DOWNTO 0);   
   SIGNAL tag_valid_s : STD_LOGIC;
            
begin


    dataout_tdata <= reverse_byte(dataout_tdata_s);
    dataout_tkeep <= reverse_bit(dataout_tkeep_s);
    dataout_tvalid <= dataout_tvalid_s;   
    dataout_tlast <= dataout_tlast_s;
    
    datain_tdata_s <= reverse_byte(datain_tdata);
    datain_tkeep_s <= reverse_bit(datain_tkeep);
    datain_tvalid_s <= datain_tvalid;   
    datain_tlast_s <= datain_tlast;   
    
    associated_data_tdata_s <= reverse_byte(associated_data_tdata);
    associated_data_tkeep_s <= reverse_bit(associated_data_tkeep);
    associated_data_tvalid_s <= associated_data_tvalid;   
    associated_data_tlast_s <= associated_data_tlast;       
    
    data_tmp <= (ascon_out1(127 DOWNTO 0) XOR datain_tdata_s) WHEN state = PROCESS_MSG AND en_dec = '1' AND datain_tlast_s = '1' ELSE
                (ascon_state(127 DOWNTO 0) XOR datain_tdata_s) WHEN state = ABSORB_MSG AND en_dec = '1' AND datain_tlast_s = '1' ELSE
                (ascon_out1(127 DOWNTO 0) XOR pad_data_in(datain_tvalid_s, datain_tlast_s, datain_tdata_s, datain_tkeep_s)) WHEN state = PROCESS_MSG AND en_dec = '0' AND datain_tlast_s = '1' ELSE
                (ascon_state(127 DOWNTO 0) XOR pad_data_in(datain_tvalid_s, datain_tlast_s, datain_tdata_s, datain_tkeep_s)) WHEN state = ABSORB_MSG AND en_dec = '0' AND datain_tlast_s = '1' ELSE                
                (others => '0');

    tag <= reverse_byte(tag_s);   
    tag_valid <= tag_valid_s;


      ascon_round: Asconp PORT MAP( state_in => ascon_state, rcon => rcon, state_out => ascon_out);
      ascon_round1: Asconp PORT MAP( state_in => ascon_out, rcon => rcon1, state_out => ascon_out1);
      
      
    ----------------------------------------------------------------------------
    --! Registers for state and internal signals
    ----------------------------------------------------------------------------
    p_reg : PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            IF (rst = '1') THEN
                state <= IDLE;
                rcon <= (others => '0');
                rcon1 <= (others => '0');
                ascon_state <= (others => '0');
                                            
            ELSE
                state <= next_state;
                rcon <= rcon_next;
                rcon1 <= rcon_next1;
                ascon_state <= ascon_state_next;
                
            END IF;
        END IF;
    END PROCESS p_reg;
    

    
p_next_state : PROCESS (state, key_valid, nonce_valid, rcon, ascon_state, ascon_out, ascon_out1, 
                        key_tmp, en_dec,
                        associated_data_tvalid_s, associated_data_tlast_s, associated_data_tdata_s, associated_data_tkeep_s,
                        datain_tvalid_s, datain_tlast_s, datain_tdata_s, datain_tkeep_s,
                        dataout_tvalid_s, dataout_tlast_s, dataout_tdata_s, dataout_tkeep_s)
    BEGIN

    -- Default values preventing latches
    next_state <= state;
    rcon_next <= rcon;
    rcon_next1 <= rcon1;
    key_init_done <= '0';
    ascon_state_next <= ascon_state;
    
    dataout_tdata_s <= (others => '0');
    dataout_tkeep_s <= (others => '0');
    dataout_tvalid_s <= '0';
    dataout_tlast_s <= '0';
    
    tag_s <= (others => '0');
    tag_valid_s <= '0';
    
    CASE state IS
        WHEN IDLE =>
            IF (key_valid = '1' AND nonce_valid = '1') THEN
                    key_tmp <= reverse_byte(key);
                    ascon_state_next(63 DOWNTO 0) <= reverse_byte(IV_AEAD);
                    ascon_state_next(191 DOWNTO 64) <= reverse_byte(key);
                    ascon_state_next(319 DOWNTO 192) <= reverse_byte(nonce);   
                    rcon_next <= X"C";
                    next_state <= INIT_SETUP;
            END IF;
        
        WHEN INIT_SETUP =>
                  if (rcon = std_logic_vector(to_unsigned(UROL,rcon'length))) then
                    ascon_state_next(319 DOWNTO 192) <= ascon_out(319 DOWNTO 192) XOR key_tmp(127 DOWNTO 0);
                    ascon_state_next(191 DOWNTO 0) <= ascon_out(191 DOWNTO 0);
                    key_init_done <= '1';
                    next_state <= ABSORB_AD;
                  else
                    rcon_next <= std_logic_vector(unsigned(rcon) - to_unsigned(UROL,rcon'length));
                    ascon_state_next <= ascon_out;
                    next_state <= INIT_SETUP;                
                  end if;
 
        WHEN ABSORB_AD =>
                  if (associated_data_tvalid_s = '1') then
                     rcon_next <= X"8";
                     rcon_next1 <= X"4";                            
                     if (associated_data_tlast_s = '1') then
                          ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR pad_data_in(associated_data_tvalid_s, associated_data_tlast_s, associated_data_tdata_s, associated_data_tkeep_s);
                          if (associated_data_tkeep_s = X"FFFF") then
                                         next_state <= EMPTY_AD; 
                          else
                                         next_state <= PROCESS_LAST_AD; 
                          end if;
                     else
                          ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR associated_data_tdata_s;
                          next_state <= PROCESS_AD;
                     end if;
                  end if;        

        WHEN PROCESS_AD =>
              if (associated_data_tvalid_s = '1') then             
                    if (associated_data_tlast_s = '1') then
                        ascon_state_next(127 DOWNTO 0) <= ascon_out1(127 DOWNTO 0) XOR pad_data_in(associated_data_tvalid_s, associated_data_tlast_s, associated_data_tdata_s, associated_data_tkeep_s);
                        ascon_state_next(319 DOWNTO 128) <= ascon_out1(319 DOWNTO 128); 
                        if (associated_data_tkeep_s = X"FFFF") then
                            next_state <= EMPTY_AD; 
                        else
                            next_state <= PROCESS_LAST_AD;
                        end if;                                  
                    else
                       ascon_state_next(127 DOWNTO 0) <= ascon_out1(127 DOWNTO 0) XOR associated_data_tdata_s;
                       ascon_state_next(319 DOWNTO 128) <= ascon_out1(319 DOWNTO 128);
                    end if;                    
                    
              end if;
              
        WHEN EMPTY_AD =>
                   ascon_state_next(319 DOWNTO 8) <= ascon_out1(319 DOWNTO 8);
                   ascon_state_next(7 DOWNTO 0) <= ascon_out1(7 DOWNTO 0) XOR X"80";
                   next_state <= PROCESS_LAST_AD;                     
              
         WHEN PROCESS_LAST_AD =>
              ascon_state_next(319 DOWNTO 312) <= ascon_out1(319 DOWNTO 312) XOR x"01";
              ascon_state_next(311 DOWNTO 0) <= ascon_out1(311 DOWNTO 0);  
              next_state <= ABSORB_MSG;
              
        WHEN ABSORB_MSG =>
              if (datain_tvalid_s = '1') then
                  rcon_next <= X"8";
                  rcon_next1 <= X"4";
                  
                  if (datain_tlast_s = '1') then
                    if (en_dec = '1') then
                        dataout_tdata_s <= trunc_data(dataout_tvalid_s, dataout_tlast_s, data_tmp, dataout_tkeep_s);
                        dataout_tvalid_s <= '1';
                        dataout_tkeep_s <= datain_tkeep_s;
                        dataout_tlast_s <= '1';
                        ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR pad_data_in(dataout_tvalid_s, dataout_tlast_s, dataout_tdata_s, dataout_tkeep_s);
                        ascon_state_next(319 DOWNTO 128) <= ascon_state(319 DOWNTO 128);
                        if (datain_tkeep_s = X"FFFF") then
                            next_state <= EMPTY_MSG; 
                        else
                            next_state <= FINAL_KEY_ADD_1; 
                        end if; 
                    else
                        dataout_tdata_s <= trunc_data(dataout_tvalid_s, dataout_tlast_s, data_tmp, dataout_tkeep_s);
                        dataout_tvalid_s <= '1';
                        dataout_tkeep_s <= datain_tkeep_s;
                        dataout_tlast_s <= '1';
                        ascon_state_next(127 DOWNTO 0) <= data_tmp;
                        ascon_state_next(319 DOWNTO 128) <= ascon_state(319 DOWNTO 128);
                        if (datain_tkeep_s = X"FFFF") then
                            next_state <= EMPTY_MSG; 
                        else
                            next_state <= FINAL_KEY_ADD_1; 
                        end if;
                    end if;
                  else
                    if (en_dec = '1') then
                        dataout_tdata_s <= ascon_state(127 DOWNTO 0) XOR datain_tdata_s;
                        dataout_tvalid_s <= '1';
                        dataout_tkeep_s <= datain_tkeep_s;
                        dataout_tlast_s <= '0';
                        ascon_state_next(127 DOWNTO 0) <= datain_tdata_s;
                        next_state <= PROCESS_MSG;
                    else
                        dataout_tdata_s <= ascon_state(127 DOWNTO 0) XOR datain_tdata_s;
                        dataout_tvalid_s <= '1';
                        dataout_tkeep_s <= datain_tkeep_s;
                        dataout_tlast_s <= '0';
                        ascon_state_next(127 DOWNTO 0) <= dataout_tdata_s;
                        next_state <= PROCESS_MSG;                  
                    end if;
                  end if;
              end if;       
                     
        WHEN PROCESS_MSG =>
                    if (datain_tvalid_s = '1') then
                          if (datain_tlast_s = '1') then
                              if (en_dec = '1') then
                                    dataout_tdata_s <= trunc_data(dataout_tvalid_s, dataout_tlast_s, data_tmp, dataout_tkeep_s);
                                    dataout_tvalid_s <= '1';
                                    dataout_tkeep_s <= datain_tkeep_s;
                                    dataout_tlast_s <= '1';
                                    ascon_state_next(127 DOWNTO 0) <= ascon_out1(127 DOWNTO 0) XOR pad_data_in(dataout_tvalid_s, dataout_tlast_s, dataout_tdata_s, dataout_tkeep_s);
                                    ascon_state_next(319 DOWNTO 128) <= ascon_out1(319 DOWNTO 128);
                                    if (datain_tkeep_s = X"FFFF") then
                                        next_state <= EMPTY_MSG;
                                    else
                                        next_state <= FINAL_KEY_ADD_1;
                                    end if;
                              else
                                  dataout_tdata_s <= trunc_data(dataout_tvalid_s, dataout_tlast_s, data_tmp, dataout_tkeep_s);
                                  dataout_tvalid_s <= '1';
                                  dataout_tkeep_s <= datain_tkeep_s;
                                  dataout_tlast_s <= '1';
                                  ascon_state_next(127 DOWNTO 0) <= data_tmp;
                                  ascon_state_next(319 DOWNTO 128) <= ascon_out1(319 DOWNTO 128);
                                  if (datain_tkeep_s = X"FFFF") then
                                      next_state <= EMPTY_MSG;
                                  else
                                      next_state <= FINAL_KEY_ADD_1;
                                  end if;
                              end if;

                          else
                              if (en_dec = '1') then
                                  dataout_tdata_s <= ascon_state(127 DOWNTO 0) XOR datain_tdata_s;
                                  dataout_tvalid_s <= '1';
                                  dataout_tkeep_s <= datain_tkeep_s;
                                  dataout_tlast_s <= '0';
                                  ascon_state_next(127 DOWNTO 0) <= datain_tdata_s;
                                  next_state <= PROCESS_MSG;                    
                              else
                                  dataout_tdata_s <= ascon_state(127 DOWNTO 0) XOR datain_tdata_s;
                                  dataout_tvalid_s <= '1';
                                  dataout_tkeep_s <= datain_tkeep_s;
                                  dataout_tlast_s <= '0';
                                  ascon_state_next(127 DOWNTO 0) <= dataout_tdata_s;
                                  next_state <= PROCESS_MSG;                                                 
                              end if;
                          end if;                    
                          
                    end if;
   

        WHEN EMPTY_MSG =>
             ascon_state_next(319 DOWNTO 8) <= ascon_out1(319 DOWNTO 8);
             ascon_state_next(7 DOWNTO 0) <= ascon_out1(7 DOWNTO 0) XOR X"80";
             next_state <= FINAL_KEY_ADD_1;                     

         WHEN FINAL_KEY_ADD_1 =>
                ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0);
                ascon_state_next(255 DOWNTO 128) <= ascon_state(255 DOWNTO 128) XOR key_tmp;
                rcon_next <= X"C";
                next_state <= FINAL_PROCESS; 

         WHEN FINAL_PROCESS =>
               IF (rcon = std_logic_vector(to_unsigned(UROL,rcon'length))) THEN
                    ascon_state_next <= ascon_out;
                    next_state <= FINAL_KEY_ADD_2;
               ELSE 
                    ascon_state_next <= ascon_out;
                    rcon_next <= std_logic_vector(unsigned(rcon) - to_unsigned(UROL,rcon'length));
                    next_state <= FINAL_PROCESS;
               END IF;   

         WHEN FINAL_KEY_ADD_2 =>
                tag_s <= ascon_state(319 DOWNTO 192) XOR key_tmp;
                tag_valid_s <= '1'; 
                next_state <= IDLE;
  
              
        WHEN OTHERS =>
            next_state <= IDLE;

    END CASE;
END PROCESS p_next_state;
    

end Behavioral;
