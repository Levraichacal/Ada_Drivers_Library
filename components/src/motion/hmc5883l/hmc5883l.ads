
------------------------------------------------------------------------------
--                             H M C 5 8 8 3 L                              --
--                               S P E C                                    --
--                                                                          --
--                     Copyright (C) 2016, Sébastien BARDOT                 --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  HMC5883L I2C device class package
--  Created by Sebastien Bardot

with Interfaces;          use Interfaces;

with HAL;                 use HAL;
with HAL.I2C;             use HAL.I2C;

package HMC5883L is

   --  Types
   type HMC5883L_Device (Port : HAL.I2C.I2C_Port_Ref) is private;

   type HMC5883L_Sample is (HMC5883L_SAMPLES_1,
                            HMC5883L_SAMPLES_2,
                            HMC5883L_SAMPLES_4,
                            HMC5883L_SAMPLES_8);
   for HMC5883L_Sample use
     (HMC5883L_SAMPLES_1 => 16#00#,
      HMC5883L_SAMPLES_2 => 16#01#,
      HMC5883L_SAMPLES_4 => 16#02#,
      HMC5883L_SAMPLES_8 => 16#03#);
   for HMC5883L_Sample'Size use 2;
   --  Type representing the sample average.
   --  See the HMC5883L Datasheet page 12.

   type HMC5883L_DataRate is (HMC5883L_DATARATE_0_75_HZ,
                              HMC5883L_DATARATE_1_5HZ,
                              HMC5883L_DATARATE_3HZ,
                              HMC5883L_DATARATE_7_5HZ,
                              HMC5883L_DATARATE_15HZ,
                              HMC5883L_DATARATE_30HZ,
                              HMC5883L_DATARATE_75HZ);
   for HMC5883L_DataRate use
   (HMC5883L_DATARATE_0_75_HZ => 16#00#,
    HMC5883L_DATARATE_1_5HZ   => 16#01#,
    HMC5883L_DATARATE_3HZ     => 16#02#,
    HMC5883L_DATARATE_7_5HZ   => 16#03#,
    HMC5883L_DATARATE_15HZ    => 16#04#,
    HMC5883L_DATARATE_30HZ    => 16#05#,
    HMC5883L_DATARATE_75HZ    => 16#06#);
   for HMC5883L_DataRate'Size use 3;
   --  Type representing the typical output rate.
   --  See the HMC5883L Datasheet page 12.

   type HMC5883L_MeasurementMode is (HMC5883L_NORMAL,
                                     HMC5883L_POSITIVE_BIAS,
                                     HMC5883L_NEGATIVE_BIAS);
   for HMC5883L_MeasurementMode use
   (HMC5883L_NORMAL        => 16#00#,
    HMC5883L_POSITIVE_BIAS => 16#01#,
    HMC5883L_NEGATIVE_BIAS => 16#02#);
   for HMC5883L_MeasurementMode'Size use 2;
   --  Type representing the measurement mode.
   --  See the HMC5883L Datasheet page 12.

   type HMC5883L_Range is (HMC5883L_RANGE_0_88GA,
                           HMC5883L_RANGE_1_3GA, --  Default
                           HMC5883L_RANGE_1_9GA,
                           HMC5883L_RANGE_2_5GA,
                           HMC5883L_RANGE_4GA,
                           HMC5883L_RANGE_4_7GA,
                           HMC5883L_RANGE_5_6GA,
                           HMC5883L_RANGE_8_1GA);
   for HMC5883L_Range use
   (HMC5883L_RANGE_0_88GA => 16#00#,
    HMC5883L_RANGE_1_3GA  => 16#01#,
    HMC5883L_RANGE_1_9GA  => 16#02#,
    HMC5883L_RANGE_2_5GA  => 16#03#,
    HMC5883L_RANGE_4GA    => 16#04#,
    HMC5883L_RANGE_4_7GA  => 16#05#,
    HMC5883L_RANGE_5_6GA  => 16#06#,
    HMC5883L_RANGE_8_1GA  => 16#07#);
   for HMC5883L_Range'Size use 3;
   --  Type representing the gains.
   --  See the HMC5883L Datasheet page 13.

   type HMC5883L_Mode is (HMC5883L_CONTINOUS,
                          HMC5883L_SINGLE,
                          HMC5883L_IDLE);
   for HMC5883L_Mode use
   (HMC5883L_CONTINOUS => 16#00#,
    HMC5883L_SINGLE    => 16#01#,
    HMC5883L_IDLE      => 16#02#); -- Also in 16#03# but only one is sufficient
   for HMC5883L_Mode'Size use 2;
   --  Type representing the operating mode.
   --  See the HMC5883L Datasheet page 14.

   --  Procedures and functions

   procedure HMC5883L_Init (Device : in out HMC5883L_Device);
   --  Initialize the HMC5883L device via the I2C.

   procedure HMC5883L_Set_Sample
     (Device : in out HMC5883L_Device;
      Sample : HMC5883L_Sample);
   --  Set the sample of the HMC5883L

   function HMC5883L_Get_Sample (Device : HMC5883L_Device) return HMC5883L_Sample;
   --  Get the sample of the HMC5883L

   procedure HMC5883L_Set_DataRate
     (Device : in out HMC5883L_Device;
      DataRate : HMC5883L_DataRate);
   --  Set the data rate of the HMC5883L

   function HMC5883L_Get_DataRate (Device : HMC5883L_Device) return HMC5883L_DataRate;
   --  Get the data rate of the HMC5883L

   procedure HMC5883L_Set_MeasurementMode
     (Device : in out HMC5883L_Device;
      MeasurementMode : HMC5883L_MeasurementMode);
   --  Set the measurement mode of the HMC5883L

   function HMC5883L_Get_MeasurementMode (Device : HMC5883L_Device) return HMC5883L_MeasurementMode;
   --  Get the measurement mode of the HMC5883L

   procedure HMC5883L_Set_Range
     (Device : in out HMC5883L_Device;
      Range_Value : HMC5883L_Range);
   --  Set the range of the HMC5883L

   function HMC5883L_Get_Range (Device : HMC5883L_Device) return HMC5883L_Range;
   --  Get the range of the HMC5883L

   procedure HMC5883L_Set_Mode
     (Device : in out HMC5883L_Device;
      Mode : HMC5883L_Mode);
   --  Set the mode of the HMC5883L

   function HMC5883L_Get_Mode (Device : HMC5883L_Device) return HMC5883L_Mode;
   --  Get the mode of the HMC5883L

   function HMC5883L_Is_Lock (Device : HMC5883L_Device) return Boolean;
   --  Get the lock of the HMC5883L

   function HMC5883L_Is_Ready (Device : HMC5883L_Device) return Boolean;
   --  Get the ready of the HMC5883L

   procedure HMC5883L_Read_Raw
     (Device : HMC5883L_Device;
      Mag_X  : out Integer_16;
      Mag_Y  : out Integer_16;
      Mag_Z  : out Integer_16);
   --  Get raw 3-axis motion sensors reading (magneto)

   procedure HMC5883L_Read_Normalize
     (Device : HMC5883L_Device;
      Mag_X_Norm  : out Float;
      Mag_Y_Norm  : out Float;
      Mag_Z_Norm  : out Float);
   --  Get raw 3-axis motion sensors normalize (magneto)

   procedure HMC5883L_Set_Offsets
     (Device : in out HMC5883L_Device;
      X_Off  : Integer_16;
      Y_Off  : Integer_16);
   --  Set the Offsets of the HMC5883L

private

   --  Type and subtype
   type HMC5883L_Device (Port : HAL.I2C.I2C_Port_Ref)
   is record
      Is_Init       : Boolean    := False;
      Mag_Per_Digit : Float      := 0.92;
      X_Offset      : Integer_16 := 0;
      Y_Offset      : Integer_16 := 0;
   end record;

   subtype T_Bit_Pos_8 is Natural  range 0 .. 7;

   --  Globals constants
   HMC5883L_ADDRESS              : constant := 16#1E#;
   HMC5883L_REG_CONFIG_A         : constant := 16#00#;
   HMC5883L_REG_CONFIG_B         : constant := 16#01#;
   HMC5883L_REG_MODE             : constant := 16#02#;
   HMC5883L_REG_OUT_X_M          : constant := 16#03#;
   HMC5883L_REG_OUT_X_L          : constant := 16#04#;
   HMC5883L_REG_OUT_Z_M          : constant := 16#05#;
   HMC5883L_REG_OUT_Z_L          : constant := 16#06#;
   HMC5883L_REG_OUT_Y_M          : constant := 16#07#;
   HMC5883L_REG_OUT_Y_L          : constant := 16#08#;
   HMC5883L_REG_STATUS           : constant := 16#09#;
   HMC5883L_REG_IDENT_A          : constant := 16#0A#;
   HMC5883L_REG_IDENT_B          : constant := 16#0B#;
   HMC5883L_REG_IDENT_C          : constant := 16#0C#;
   --  The HMC5883L Registers.
   --  See the HMC5883L Datasheet page 11.

   HMC5883L_SAMPLE_BIT           : constant := 5;
   HMC5883L_SAMPLE_LENGTH        : constant := 2;
   HMC5883L_DATARATE_BIT         : constant := 2;
   HMC5883L_DATARATE_LENGTH      : constant := 3;
   HMC5883L_MEASUREMENT_BIT      : constant := 0;
   HMC5883L_MEASUREMENT_LENGTH   : constant := 2;
   --  Bits in configuration register A.
   --  See the HMC5883L Datasheet page 12.

   HMC5883L_RANGE_BIT            : constant := 5;
   HMC5883L_RANGE_LENGTH         : constant := 3;
   --  Bits in configuration register B.
   --  See the HMC5883L Datasheet page 13.

   HMC5883L_MODE_BIT             : constant := 0;
   HMC5883L_MODE_LENGTH          : constant := 2;
   --  Bits in mode register.
   --  See the HMC5883L Datasheet page 14.

   HMC5883L_LOCK_BIT             : constant := 1;
--   HMC5883L_LOCK_LENGTH          : constant := 1;
   HMC5883L_RDY_BIT              : constant := 0;
--   HMC5883L_RDY_LENGTH           : constant := 1;
   --  Bits in status registers.
   --  See the HMC5883L Datasheet page 16.

   --  Procedures and functions

   procedure HMC5883L_Read_Register
     (Device   : HMC5883L_Device;
      Reg_Addr    : Byte;
      Data        : in out I2C_Data);
   --  Read data to the specified HMC5883L register

   procedure HMC5883L_Read_Byte_At_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Data     : out Byte);
   --  Read one byte at the specified HMC5883L register

   function HMC5883L_Read_Bit_At_Register
     (Device    : HMC5883L_Device;
      Reg_Addr  : Byte;
      Bit_Pos   : T_Bit_Pos_8) return Boolean;
   --  Read one but at the specified HMC5883L register

   procedure HMC5883L_Write_Register
     (Device      : HMC5883L_Device;
      Reg_Addr    : Byte;
      Data        : I2C_Data);
   --  Write data to the specified HMC5883L register

   procedure HMC5883L_Write_Byte_At_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Data     : Byte);
   --  Write one byte at the specified HMC5883L register

   procedure HMC5883L_Write_Bit_At_Register
     (Device    : HMC5883L_Device;
      Reg_Addr  : Byte;
      Bit_Pos   : T_Bit_Pos_8;
      Bit_Value : Boolean);
   --  Write one bit at the specified HMC5883L register

   procedure HMC5883L_Write_Bits_At_Register
     (Device    : HMC5883L_Device;
      Reg_Addr      : Byte;
      Start_Bit_Pos : T_Bit_Pos_8;
      Data          : Byte;
      Length        : T_Bit_Pos_8);
   --  Write data in the specified register, starting from the
   --  bit specified in Start_Bit_Pos

   function Fuse_Low_And_High_Register_Parts
     (High : Byte;
      Low  : Byte) return Integer_16;
   pragma Inline (Fuse_Low_And_High_Register_Parts);
   --  Fusion High and Low register parts

end HMC5883L;
