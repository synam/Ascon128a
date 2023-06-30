
--------------------------------------------------------------------------------
--! @file       CryptoCore.vhd
--! @brief      Implementation of Ascon-128, Ascon-128a and Ascon-Hash.
--!
--! @author     Robert Primas <rprimas@protonmail.com>
--! @copyright  Copyright (c) 2020 IAIK, Graz University of Technology, AUSTRIA
--!             All rights Reserved.
--! @license    This project is released under the GNU Public License.          
--!             The license and distribution terms for this file may be         
--!             found in the file LICENSE in this distribution or at            
--!             http://www.gnu.org/licenses/gpl-3.0.txt                         
--! @note       This is publicly available encryption source code that falls    
--!             under the License Exception TSU (Technology and software-       
--!             unrestricted)                                                  
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;
USE ieee.std_logic_misc.ALL;
USE work.design_pkg.ALL;


ENTITY Asconp IS
    PORT (
        state_in : IN STD_LOGIC_VECTOR(319 DOWNTO 0);
        rcon : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        state_out : OUT STD_LOGIC_VECTOR(319 DOWNTO 0)
    );
END;

ARCHITECTURE behavior OF Asconp IS
    CONSTANT rounds_16 : STD_LOGIC_VECTOR(3 DOWNTO 0) := X"F";
    CONSTANT rounds_12 : STD_LOGIC_VECTOR(3 DOWNTO 0) := X"C";
    
BEGIN

    PROCESS (state_in, rcon)
        VARIABLE x0, x1, x2, x3, x4 : STD_LOGIC_VECTOR(63 DOWNTO 0);
        VARIABLE t0, t1 : STD_LOGIC_VECTOR(63 DOWNTO 0);
        VARIABLE t2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
        
    BEGIN
        ---------------------------------------------------------------------------
        --! Map bit vector to ascon state
        ---------------------------------------------------------------------------
        -- Map 320-bit vector to 5 x 64-bit lanes.
        x0 := state_in(63 + 0 * 64 DOWNTO 0 * 64);
        x1 := state_in(63 + 1 * 64 DOWNTO 1 * 64);
        x2 := state_in(63 + 2 * 64 DOWNTO 2 * 64);
        x3 := state_in(63 + 3 * 64 DOWNTO 3 * 64);
        x4 := state_in(63 + 4 * 64 DOWNTO 4 * 64);
        
        -- Endian swap.
        x0 := reverse_byte(x0);
        x1 := reverse_byte(x1);
        x2 := reverse_byte(x2);
        x3 := reverse_byte(x3);
        x4 := reverse_byte(x4);

        ---------------------------------------------------------------------------
        --! Ascon Round
        ---------------------------------------------------------------------------
        for r in 1 to UROL loop     

            -- Linear operations and addition of round constant.
            t2 := std_logic_vector(unsigned(rounds_12) - unsigned(rcon) + r -1);
            x0 := x0 XOR x4;
            x2(7 DOWNTO 0) := x2(7 DOWNTO 0) XOR x1(7 DOWNTO 0) XOR (std_logic_vector(unsigned(rounds_16) - unsigned(t2)) & t2);
            x2(63 DOWNTO 8) := x2(63 DOWNTO 8) XOR x1(63 DOWNTO 8);
            x4 := x4 XOR x3;
    
            -- Nonlinear operations, same as used in Keccak-Sbox
            t0 := x0;
            t1 := x1;
            x0 := x0 XOR (NOT x1 AND x2);
            x1 := x1 XOR (NOT x2 AND x3);
            x2 := x2 XOR (NOT x3 AND x4);
            x3 := x3 XOR (NOT x4 AND t0);
            x4 := x4 XOR (NOT t0 AND t1);
    
            -- Linear operations.
            x1 := x1 XOR x0;
            x3 := x3 XOR x2;
            x0 := x0 XOR x4;
            x2 := NOT x2;
    
            -- Lane rotations.
            x0 := x0 XOR (x0(18 DOWNTO 0) & x0(63 DOWNTO 19)) XOR (x0(27 DOWNTO 0) & x0(63 DOWNTO 28));
            x1 := x1 XOR (x1(60 DOWNTO 0) & x1(63 DOWNTO 61)) XOR (x1(38 DOWNTO 0) & x1(63 DOWNTO 39));
            x2 := x2 XOR (x2(0 DOWNTO 0) & x2(63 DOWNTO 1)) XOR (x2(5 DOWNTO 0) & x2(63 DOWNTO 6));
            x3 := x3 XOR (x3(9 DOWNTO 0) & x3(63 DOWNTO 10)) XOR (x3(16 DOWNTO 0) & x3(63 DOWNTO 17));
            x4 := x4 XOR (x4(6 DOWNTO 0) & x4(63 DOWNTO 7)) XOR (x4(40 DOWNTO 0) & x4(63 DOWNTO 41));
        
        end loop;
        
        ---------------------------------------------------------------------------
        --! Map ascon state to bit vector
        ---------------------------------------------------------------------------
        -- Endian swap.
        x0 := reverse_byte(x0);
        x1 := reverse_byte(x1);
        x2 := reverse_byte(x2);
        x3 := reverse_byte(x3);
        x4 := reverse_byte(x4);

        -- Map 5 x 64-bit lanes to 320-bit vector.
        state_out(63 + 0 * 64 DOWNTO 0 * 64) <= x0;
        state_out(63 + 1 * 64 DOWNTO 1 * 64) <= x1;
        state_out(63 + 2 * 64 DOWNTO 2 * 64) <= x2;
        state_out(63 + 3 * 64 DOWNTO 3 * 64) <= x3;
        state_out(63 + 4 * 64 DOWNTO 4 * 64) <= x4;
    END PROCESS;
END;
