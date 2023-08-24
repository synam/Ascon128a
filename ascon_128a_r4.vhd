--------------------------------------------------------------------------------
--! @file       ascon_128a_r4.vhd
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

entity ascon_128a_r4 is
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
        associated_data_tready : OUT STD_LOGIC;
        datain_tdata : IN STD_LOGIC_VECTOR (127 DOWNTO 0);
        datain_tkeep : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
        datain_tvalid : IN STD_LOGIC;
        datain_tlast : IN STD_LOGIC;
        datain_tready : OUT STD_LOGIC;
        dataout_tdata : OUT STD_LOGIC_VECTOR (127 DOWNTO 0);
        dataout_tkeep : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
        dataout_tvalid : OUT STD_LOGIC;
        dataout_tlast : OUT STD_LOGIC;
        dataout_tready : IN STD_LOGIC;
        key_init_done : OUT STD_LOGIC;
        tag : OUT STD_LOGIC_VECTOR (127 DOWNTO 0);   
        tag_valid : OUT STD_LOGIC
    );
end ascon_128a_r4;

architecture Behavioral of ascon_128a is

        COMPONENT Asconp
            PORT (
                        state_in : IN STD_LOGIC_VECTOR(319 DOWNTO 0);
                        rcon : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
                        state_out : OUT STD_LOGIC_VECTOR(319 DOWNTO 0)
                  );
        END COMPONENT;

        COMPONENT  fallthrough_small_fifo
            GENERIC (
                        WIDTH : integer := 128;
                        MAX_DEPTH_BITS : integer := 5
                    );
            
            PORT (
                        din : IN STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
                        wr_en : IN STD_LOGIC;
                        rd_en: IN STD_LOGIC;
                        dout : OUT STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
                        full : OUT STD_LOGIC;
                        nearly_full : OUT STD_LOGIC;
                        prog_full : OUT STD_LOGIC;
                        empty : OUT STD_LOGIC;
                        
                        clk : IN STD_LOGIC;
                        reset : IN STD_LOGIC                      
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

   SIGNAL associated_data_fifo_in : STD_LOGIC_VECTOR (144 DOWNTO 0); 
   SIGNAL associated_data_fifo_out : STD_LOGIC_VECTOR (144 DOWNTO 0);         
   SIGNAL associated_data_tdata_fifo_out : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL associated_data_tkeep_fifo_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
   SIGNAL associated_data_tlast_fifo_out : STD_LOGIC;            
         
   SIGNAL associated_data_rd_en : STD_LOGIC; 
   SIGNAL associated_data_full : STD_LOGIC;  
   SIGNAL associated_data_nearly_full : STD_LOGIC;         
   SIGNAL associated_data_prog_full : STD_LOGIC;    
   SIGNAL associated_data_empty : STD_LOGIC;                

   SIGNAL datain_fifo_in : STD_LOGIC_VECTOR (144 DOWNTO 0); 
   SIGNAL datain_fifo_out : STD_LOGIC_VECTOR (144 DOWNTO 0);         
   SIGNAL datain_tdata_fifo_out : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL datain_tkeep_fifo_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
   SIGNAL datain_tlast_fifo_out : STD_LOGIC;            
         
   SIGNAL datain_rd_en : STD_LOGIC; 
   SIGNAL datain_full : STD_LOGIC;  
   SIGNAL datain_nearly_full : STD_LOGIC;         
   SIGNAL datain_prog_full : STD_LOGIC;    
   SIGNAL datain_empty : STD_LOGIC;         


   SIGNAL pad_empty, pad_empty_next : STD_LOGIC;      
   SIGNAL last_ad, last_ad_next : STD_LOGIC;  

   SIGNAL data_tmp : STD_LOGIC_VECTOR (127 DOWNTO 0);
   SIGNAL tag_s : STD_LOGIC_VECTOR (127 DOWNTO 0);   
   SIGNAL tag_valid_s : STD_LOGIC;
          
   SIGNAL x0 : STD_LOGIC_VECTOR (63 DOWNTO 0);       
   SIGNAL x1 : STD_LOGIC_VECTOR (63 DOWNTO 0);
   SIGNAL x2 : STD_LOGIC_VECTOR (63 DOWNTO 0);
   SIGNAL x3 : STD_LOGIC_VECTOR (63 DOWNTO 0);       
   SIGNAL x4 : STD_LOGIC_VECTOR (63 DOWNTO 0); 
   
   SIGNAL x0_s : STD_LOGIC_VECTOR (63 DOWNTO 0);       
   SIGNAL x1_s : STD_LOGIC_VECTOR (63 DOWNTO 0);
   SIGNAL x2_s : STD_LOGIC_VECTOR (63 DOWNTO 0);
   SIGNAL x3_s : STD_LOGIC_VECTOR (63 DOWNTO 0);       
   SIGNAL x4_s : STD_LOGIC_VECTOR (63 DOWNTO 0);  
 
            
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
    
--    data_tmp <= (ascon_out1(127 DOWNTO 0) XOR datain_tdata_s) WHEN state = PROCESS_MSG AND en_dec = '1' AND datain_tlast_s = '1' ELSE
--                (ascon_state(127 DOWNTO 0) XOR datain_tdata_s) WHEN state = ABSORB_MSG AND en_dec = '1' AND datain_tlast_s = '1' ELSE
--                (ascon_out1(127 DOWNTO 0) XOR pad_data_in(datain_tvalid_s, datain_tlast_s, datain_tdata_s, datain_tkeep_s)) WHEN state = PROCESS_MSG AND en_dec = '0' AND datain_tlast_s = '1' ELSE
--                (ascon_state(127 DOWNTO 0) XOR pad_data_in(datain_tvalid_s, datain_tlast_s, datain_tdata_s, datain_tkeep_s)) WHEN state = ABSORB_MSG AND en_dec = '0' AND datain_tlast_s = '1' ELSE                
--                (others => '0');

    data_tmp <= (ascon_state(127 DOWNTO 0) XOR datain_tdata_fifo_out) WHEN state = ABSORB_MSG AND en_dec = '1' AND datain_tlast_fifo_out = '1' ELSE
                (ascon_state(127 DOWNTO 0) XOR pad_data_in(datain_rd_en, datain_tlast_fifo_out, datain_tdata_fifo_out, datain_tkeep_fifo_out)) WHEN state = ABSORB_MSG AND en_dec = '0' AND datain_tlast_fifo_out = '1' ELSE                
                (others => '0');

    tag <= reverse_byte(tag_s);   
    tag_valid <= tag_valid_s;


    x0_s <= ascon_state(63 DOWNTO 0);
    x1_s <= ascon_state(127 DOWNTO 64);
    x2_s <= ascon_state(191 DOWNTO 128);
    x3_s <= ascon_state(255 DOWNTO 192);
    x4_s <= ascon_state(319 DOWNTO 256);

    x0 <= reverse_byte(x0_s);
    x1 <= reverse_byte(x1_s);
    x2 <= reverse_byte(x2_s);
    x3 <= reverse_byte(x3_s);
    x4 <= reverse_byte(x4_s);

      ascon_round: Asconp PORT MAP( state_in => ascon_state, rcon => rcon, state_out => ascon_out);


    associated_data_fifo_in <= associated_data_tdata_s & associated_data_tkeep_s & associated_data_tlast_s;
    datain_fifo_in <= datain_tdata_s & datain_tkeep_s & datain_tlast_s;


    associate_fifo : fallthrough_small_fifo 
    generic map (
         WIDTH => 145,
         MAX_DEPTH_BITS => 5
    )
    port map (
         din => associated_data_fifo_in,
         wr_en => associated_data_tvalid_s,
         rd_en => associated_data_rd_en,
         dout => associated_data_fifo_out,
         full => associated_data_full,
         nearly_full => associated_data_nearly_full,
         prog_full => associated_data_prog_full,
         empty => associated_data_empty,
         clk => clk,
         reset => rst
     );

    datain_fifo : fallthrough_small_fifo 
    generic map (
         WIDTH => 145,
         MAX_DEPTH_BITS => 5
    )
    port map (
         din => datain_fifo_in,
         wr_en => datain_tvalid_s,
         rd_en => datain_rd_en,
         dout => datain_fifo_out,
         full => datain_full,
         nearly_full => datain_nearly_full,
         prog_full => datain_prog_full,
         empty => datain_empty,
         clk => clk,
         reset => rst
     );

    associated_data_tready <= not associated_data_nearly_full;
    associated_data_tdata_fifo_out <= associated_data_fifo_out(144 DOWNTO 17);
    associated_data_tkeep_fifo_out <= associated_data_fifo_out(16 DOWNTO 1);
    associated_data_tlast_fifo_out <= associated_data_fifo_out(0); 
    

    datain_tready <= not datain_nearly_full;
    datain_tdata_fifo_out <= datain_fifo_out(144 DOWNTO 17);
    datain_tkeep_fifo_out <= datain_fifo_out(16 DOWNTO 1);
    datain_tlast_fifo_out <= datain_fifo_out(0); 
            
    
    ----------------------------------------------------------------------------
    --! Registers for state and internal signals
    ----------------------------------------------------------------------------
    p_reg : PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            IF (rst = '1') THEN
                state <= IDLE;
                rcon <= (others => '0');
                ascon_state <= (others => '0');
                pad_empty <= '0';
                last_ad <= '0';
                                            
            ELSE
                state <= next_state;
                rcon <= rcon_next;
                ascon_state <= ascon_state_next;
                pad_empty <= pad_empty_next;
                last_ad <= last_ad_next;
            END IF;
        END IF;
    END PROCESS p_reg;
    

    
p_next_state : PROCESS (state, key_valid, nonce_valid, rcon, ascon_state, ascon_out, ascon_out1, 
                        key_tmp, data_tmp, en_dec, pad_empty, last_ad,
                        associated_data_rd_en, associated_data_empty, associated_data_tlast_fifo_out,
                        datain_rd_en, datain_empty, datain_tlast_fifo_out,
                        associated_data_tvalid_s, associated_data_tlast_s, associated_data_tdata_s, associated_data_tkeep_s,
                        datain_tvalid_s, datain_tlast_s, datain_tdata_s, datain_tkeep_s,
                        dataout_tvalid_s, dataout_tlast_s, dataout_tdata_s, dataout_tkeep_s)
    BEGIN

    -- Default values preventing latches
    next_state <= state;
    rcon_next <= rcon;
    pad_empty_next <= pad_empty;
    last_ad_next <= last_ad;
    key_init_done <= '0';
    ascon_state_next <= ascon_state;
    
    dataout_tdata_s <= (others => '0');
    dataout_tkeep_s <= (others => '0');
    dataout_tvalid_s <= '0';
    dataout_tlast_s <= '0';
    
    tag_s <= (others => '0');
    tag_valid_s <= '0';
    
    associated_data_rd_en <= '0';
    datain_rd_en <= '0';
    
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
                    if (associated_data_empty = '0') then
                        associated_data_rd_en <= '1';
                        rcon_next <= X"8";
                        if (associated_data_tlast_fifo_out = '1') then
                            ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR pad_data_in(associated_data_rd_en, associated_data_tlast_fifo_out, associated_data_tdata_fifo_out, associated_data_tkeep_fifo_out);
                            if (associated_data_tkeep_fifo_out = X"FFFF") then
                                pad_empty_next <= '1';
                                next_state <= PROCESS_AD;
                            else
                                last_ad_next <= '1';
                                next_state <= PROCESS_AD; 
                            end if;
                        else 
                            ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR associated_data_tdata_fifo_out;
                            next_state <= PROCESS_AD;
                        end if;
                        
                    end if;
   
        WHEN PROCESS_AD =>
                  if (rcon = std_logic_vector(to_unsigned(UROL,rcon'length))) then
                      ascon_state_next <= ascon_out;
                      if (pad_empty = '1') then
                            pad_empty_next <= '0';
                            next_state <= EMPTY_AD;
                      elsif (last_ad = '1') then
                            last_ad_next <= '0';
                            next_state <= PROCESS_LAST_AD;
                      else
                            next_state <= ABSORB_AD;
                      end if;
                   else
                      rcon_next <= std_logic_vector(unsigned(rcon) - to_unsigned(UROL,rcon'length));
                      ascon_state_next <= ascon_out;
                      next_state <= PROCESS_AD;                
                   end if;        
        
 
        WHEN EMPTY_AD =>
                   ascon_state_next(319 DOWNTO 8) <= ascon_state(319 DOWNTO 8);
                   ascon_state_next(7 DOWNTO 0) <= ascon_state(7 DOWNTO 0) XOR X"80";     
                   last_ad_next <= '1';
                   rcon_next <= X"8";
                   next_state <= PROCESS_AD;          
        
        WHEN PROCESS_LAST_AD =>
                   ascon_state_next(319 DOWNTO 312) <= ascon_state(319 DOWNTO 312) XOR x"01";
                   next_state <= ABSORB_MSG;        
          
        
        WHEN ABSORB_MSG =>
                if (datain_empty = '0') and (dataout_tready = '1') then
                    datain_rd_en <= '1';
                    rcon_next <= X"8";
                    if (datain_tlast_fifo_out = '1') then
                        if (en_dec = '1') then
                            dataout_tdata_s <= trunc_data(dataout_tvalid_s, dataout_tlast_s, data_tmp, dataout_tkeep_s);
                            dataout_tvalid_s <= '1';
                            dataout_tkeep_s <= datain_tkeep_fifo_out;
                            dataout_tlast_s <= '1';
                            ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR pad_data_in(dataout_tvalid_s, dataout_tlast_s, dataout_tdata_s, dataout_tkeep_s);
                            ascon_state_next(319 DOWNTO 128) <= ascon_state(319 DOWNTO 128);
                            if (datain_tkeep_fifo_out = X"FFFF") then
                                    pad_empty_next <= '1';
                                    next_state <= PROCESS_MSG; 
                            else
                                    next_state <= FINAL_KEY_ADD_1; 
                            end if;                             
                        else
                            dataout_tdata_s <= trunc_data(dataout_tvalid_s, dataout_tlast_s, data_tmp, dataout_tkeep_s);
                            dataout_tvalid_s <= '1';
                            dataout_tkeep_s <= datain_tkeep_fifo_out;
                            dataout_tlast_s <= '1';
                            ascon_state_next(127 DOWNTO 0) <= data_tmp;
                            ascon_state_next(319 DOWNTO 128) <= ascon_state(319 DOWNTO 128);
                            if (datain_tkeep_fifo_out = X"FFFF") then
                                pad_empty_next <= '1';
                                next_state <= PROCESS_MSG; 
                            else
                                next_state <= FINAL_KEY_ADD_1;
                            end if;                            
                        end if;
                    else
                        if (en_dec = '1') then
                            dataout_tdata_s <= ascon_state(127 DOWNTO 0) XOR datain_tdata_fifo_out;
                            dataout_tvalid_s <= '1';
                            dataout_tkeep_s <= datain_tkeep_fifo_out;
                            dataout_tlast_s <= '0';
                            ascon_state_next(127 DOWNTO 0) <= datain_tdata_fifo_out;
                            next_state <= PROCESS_MSG;
                        else
                            dataout_tdata_s <= ascon_state(127 DOWNTO 0) XOR datain_tdata_fifo_out;
                            dataout_tvalid_s <= '1';
                            dataout_tkeep_s <= datain_tkeep_fifo_out;
                            dataout_tlast_s <= '0';   
                            ascon_state_next(127 DOWNTO 0) <= ascon_state(127 DOWNTO 0) XOR datain_tdata_fifo_out;
                            next_state <= PROCESS_MSG;                                                     
                        end if;
                    end if;                
                end if;
        
        
        
        WHEN PROCESS_MSG =>
                   if (rcon = std_logic_vector(to_unsigned(UROL,rcon'length))) then
                              ascon_state_next <= ascon_out;
                              if (pad_empty = '1') then
                                    pad_empty_next <= '0';
                                    next_state <= EMPTY_MSG;                              
                              else
                                    next_state <= ABSORB_MSG;
                              end if;
                           else
                              rcon_next <= std_logic_vector(unsigned(rcon) - to_unsigned(UROL,rcon'length));
                              ascon_state_next <= ascon_out;
                              next_state <= PROCESS_MSG;                
                    end if;   
                       
        WHEN EMPTY_MSG =>
                    ascon_state_next(319 DOWNTO 8) <= ascon_state(319 DOWNTO 8);
                    ascon_state_next(7 DOWNTO 0) <= ascon_state(7 DOWNTO 0) XOR X"80";
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
