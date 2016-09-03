------------------------------------------------------------------------------
--                             H M C 5 8 8 3 L                              --
--                               B O D Y                                    --
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

with Ada.Unchecked_Conversion;

package body HMC5883L is

   -------------------
   -- HMC5883L_Init --
   -------------------

   procedure HMC5883L_Init (Device : in out HMC5883L_Device) is
   begin
      if Device.Is_Init then
         return;
      end if;
      Device.Is_Init := True;
   end HMC5883L_Init;

   -------------------------
   -- HMC5883L_Set_Sample --
   -------------------------

   procedure HMC5883L_Set_Sample
     (Device : in out HMC5883L_Device;
      Sample : HMC5883L_Sample) is
   begin
      HMC5883L_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => HMC5883L_REG_CONFIG_A,
         Start_Bit_Pos => HMC5883L_SAMPLE_BIT,
         Data          => HMC5883L_Sample'Enum_Rep (Sample),
         Length        => HMC5883L_SAMPLE_LENGTH);
   end HMC5883L_Set_Sample;

   -------------------------
   -- HMC5883L_Get_Sample --
   -------------------------

   function HMC5883L_Get_Sample (Device : HMC5883L_Device) return HMC5883L_Sample is
      Raw_Data : I2C_Data (1 .. 1);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_CONFIG_A,
         Data     => Raw_Data);
      return HMC5883L_Sample'Val (Shift_Left ((Raw_Data (1) and 2#01100000#), HMC5883L_SAMPLE_BIT));
   end HMC5883L_Get_Sample;

   ---------------------------
   -- HMC5883L_Set_DataRate --
   ---------------------------

   procedure HMC5883L_Set_DataRate
     (Device : in out HMC5883L_Device;
      DataRate : HMC5883L_DataRate) is
   begin
      HMC5883L_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => HMC5883L_REG_CONFIG_A,
         Start_Bit_Pos => HMC5883L_DATARATE_BIT,
         Data          => HMC5883L_DataRate'Enum_Rep (DataRate),
         Length        => HMC5883L_DATARATE_LENGTH);
   end HMC5883L_Set_DataRate;

   ---------------------------
   -- HMC5883L_Get_DataRate --
   ---------------------------

   function HMC5883L_Get_DataRate (Device : HMC5883L_Device) return HMC5883L_DataRate is
      Raw_Data : I2C_Data (1 .. 1);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_CONFIG_A,
         Data     => Raw_Data);
      return HMC5883L_DataRate'Val (Shift_Left ((Raw_Data (1) and 2#00011100#), HMC5883L_DATARATE_BIT));
   end HMC5883L_Get_DataRate;

   ----------------------------------
   -- HMC5883L_Set_MeasurementMode --
   ----------------------------------

   procedure HMC5883L_Set_MeasurementMode
     (Device : in out HMC5883L_Device;
      MeasurementMode : HMC5883L_MeasurementMode) is
   begin
      HMC5883L_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => HMC5883L_REG_CONFIG_A,
         Start_Bit_Pos => HMC5883L_MEASUREMENT_BIT,
         Data          => HMC5883L_MeasurementMode'Enum_Rep (MeasurementMode),
         Length        => HMC5883L_MEASUREMENT_LENGTH);
   end HMC5883L_Set_MeasurementMode;

   ----------------------------------
   -- HMC5883L_Get_MeasurementMode --
   ----------------------------------

   function HMC5883L_Get_MeasurementMode (Device : HMC5883L_Device) return HMC5883L_MeasurementMode is
      Raw_Data : I2C_Data (1 .. 1);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_CONFIG_A,
         Data     => Raw_Data);
      return HMC5883L_MeasurementMode'Val ((Raw_Data (1) and 2#00000011#));
   end HMC5883L_Get_MeasurementMode;

   ------------------------
   -- HMC5883L_Set_Range --
   ------------------------

   procedure HMC5883L_Set_Range
     (Device : in out HMC5883L_Device;
      Range_Value : HMC5883L_Range) is
   begin
      HMC5883L_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => HMC5883L_REG_CONFIG_B,
         Start_Bit_Pos => HMC5883L_RANGE_BIT,
         Data          => HMC5883L_Range'Enum_Rep (Range_Value),
         Length        => HMC5883L_RANGE_LENGTH);
   end HMC5883L_Set_Range;

   ------------------------
   -- HMC5883L_Get_Range --
   ------------------------

   function HMC5883L_Get_Range (Device : HMC5883L_Device) return HMC5883L_Range is
      Raw_Data : I2C_Data (1 .. 1);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_CONFIG_B,
         Data     => Raw_Data);
      return HMC5883L_Range'Val (Shift_Left ((Raw_Data (1) and 2#11100000#), HMC5883L_SAMPLE_BIT));
   end HMC5883L_Get_Range;

   -----------------------
   -- HMC5883L_Set_Mode --
   -----------------------

   procedure HMC5883L_Set_Mode
     (Device : in out HMC5883L_Device;
      Mode : HMC5883L_Mode) is
   begin
      HMC5883L_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => HMC5883L_REG_MODE,
         Start_Bit_Pos => HMC5883L_MODE_BIT,
         Data          => HMC5883L_Mode'Enum_Rep (Mode),
         Length        => HMC5883L_MODE_LENGTH);
   end HMC5883L_Set_Mode;

   -----------------------
   -- HMC5883L_Get_Mode --
   -----------------------

   function HMC5883L_Get_Mode (Device : HMC5883L_Device) return HMC5883L_Mode is
      Raw_Data : I2C_Data (1 .. 1);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_MODE,
         Data     => Raw_Data);
      return HMC5883L_Mode'Val ((Raw_Data (1) and 2#00000011#));
   end HMC5883L_Get_Mode;

   ----------------------
   -- HMC5883L_Is_Lock --
   ----------------------

   function HMC5883L_Is_Lock (Device : HMC5883L_Device) return Boolean is
   begin
      return HMC5883L_Read_Bit_At_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_STATUS,
         Bit_Pos  => HMC5883L_LOCK_BIT);
   end HMC5883L_Is_Lock;

   -----------------------
   -- HMC5883L_Is_Ready --
   -----------------------

   function HMC5883L_Is_Ready (Device : HMC5883L_Device) return Boolean is
   begin
      return HMC5883L_Read_Bit_At_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_STATUS,
         Bit_Pos  => HMC5883L_RDY_BIT);
   end HMC5883L_Is_Ready;

   -----------------------
   -- HMC5883L_Read_Raw --
   -----------------------

   procedure HMC5883L_Read_Raw
     (Device : HMC5883L_Device;
      Mag_X  : out Integer_16;
      Mag_Y  : out Integer_16;
      Mag_Z  : out Integer_16)
   is
      Raw_Data : I2C_Data (1 .. 6);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_OUT_X_M,
         Data     => Raw_Data);
      Mag_X :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (1), Raw_Data (2)) - Device.X_Offset;
      Mag_Y :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (3), Raw_Data (4)) - Device.Y_Offset;
      Mag_Z :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (5), Raw_Data (6));
   end HMC5883L_Read_Raw;

   -----------------------------
   -- HMC5883L_Read_Normalize --
   -----------------------------

   procedure HMC5883L_Read_Normalize
     (Device     : HMC5883L_Device;
      Mag_X_Norm : out Float;
      Mag_Y_Norm : out Float;
      Mag_Z_Norm : out Float)
   is
      Raw_Data : I2C_Data (1 .. 6);
   begin
      HMC5883L_Read_Register
        (Device   => Device,
         Reg_Addr => HMC5883L_REG_OUT_X_M,
         Data     => Raw_Data);
      Mag_X_Norm := (Float (Fuse_Low_And_High_Register_Parts (Raw_Data (1), Raw_Data (2))
                     - Device.X_Offset)) * Device.Mag_Per_Digit;
      Mag_Y_Norm := (Float (Fuse_Low_And_High_Register_Parts (Raw_Data (3), Raw_Data (4))
                     - Device.Y_Offset)) * Device.Mag_Per_Digit;
      Mag_Z_Norm := (Float (Fuse_Low_And_High_Register_Parts (Raw_Data (5), Raw_Data (6))))
                     * Device.Mag_Per_Digit;
   end HMC5883L_Read_Normalize;

   --------------------------
   -- HMC5883L_Set_Offsets --
   --------------------------

   procedure HMC5883L_Set_Offsets
     (Device : in out HMC5883L_Device;
      X_Off  : Integer_16;
      Y_Off  : Integer_16) is
   begin
      Device.X_Offset := X_Off;
      Device.Y_Offset := Y_Off;
   end HMC5883L_Set_Offsets;


   --  Privates procedures and functions

   ---------------------------
   -- HMC5883L_Read_Register --
   ---------------------------

   procedure HMC5883L_Read_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Data     : in out I2C_Data)
   is
      Status : I2C_Status;
   begin
      Device.Port.Mem_Read
        (Addr          => HMC5883L_ADDRESS,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Data,
         Status        => Status);
   end HMC5883L_Read_Register;

   -----------------------------------
   -- HMC5883L_Read_Byte_At_Register --
   -----------------------------------

   procedure HMC5883L_Read_Byte_At_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Data     : out Byte)
   is
      Status : I2C_Status;
      I_Data : I2C_Data (1 .. 1);
   begin
      Device.Port.Mem_Read
        (Addr          => HMC5883L_ADDRESS,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => I_Data,
         Status        => Status);
      Data := I_Data (1);
   end HMC5883L_Read_Byte_At_Register;

   ----------------------------------
   -- HMC5883L_Read_Bit_At_Register --
   ----------------------------------

   function HMC5883L_Read_Bit_At_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Bit_Pos  : T_Bit_Pos_8) return Boolean
   is
      Register_Value : Byte;
   begin
      HMC5883L_Read_Byte_At_Register (Device, Reg_Addr, Register_Value);

      return (Register_Value and Shift_Left (1, Bit_Pos)) /= 0;
   end HMC5883L_Read_Bit_At_Register;

   ----------------------------
   -- HMC5883L_Write_Register --
   ----------------------------

   procedure HMC5883L_Write_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Data     : I2C_Data)
   is
      Status : I2C_Status;
   begin
      Device.Port.Mem_Write
        (Addr          => HMC5883L_ADDRESS,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Data,
         Status        => Status);
   end HMC5883L_Write_Register;

   ------------------------------------
   -- HMC5883L_Write_Byte_At_Register --
   ------------------------------------

   procedure HMC5883L_Write_Byte_At_Register
     (Device   : HMC5883L_Device;
      Reg_Addr : Byte;
      Data     : Byte)
   is
      Status : I2C_Status;
   begin
      Device.Port.Mem_Write
        (Addr          => HMC5883L_ADDRESS,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => (1 => Data),
         Status        => Status);
   end HMC5883L_Write_Byte_At_Register;

   -----------------------------------
   -- HMC5883L_Write_Bit_At_Register --
   -----------------------------------

   procedure HMC5883L_Write_Bit_At_Register
     (Device    : HMC5883L_Device;
      Reg_Addr  : Byte;
      Bit_Pos   : T_Bit_Pos_8;
      Bit_Value : Boolean)
   is
      Register_Value : Byte;
   begin
      HMC5883L_Read_Byte_At_Register (Device, Reg_Addr, Register_Value);

      Register_Value := (if Bit_Value then
                            Register_Value or (Shift_Left (1, Bit_Pos))
                         else
                            Register_Value and not (Shift_Left (1, Bit_Pos)));

      HMC5883L_Write_Byte_At_Register (Device, Reg_Addr, Register_Value);
   end HMC5883L_Write_Bit_At_Register;

   ------------------------------------
   -- HMC5883L_Write_Bits_At_Register --
   ------------------------------------

   procedure HMC5883L_Write_Bits_At_Register
     (Device        : HMC5883L_Device;
      Reg_Addr      : Byte;
      Start_Bit_Pos : T_Bit_Pos_8;
      Data          : Byte;
      Length        : T_Bit_Pos_8)
   is
      Register_Value : Byte;
      Mask           : Byte;
      Data_Aux       : Byte := Data;
   begin
      HMC5883L_Read_Byte_At_Register (Device, Reg_Addr, Register_Value);

      Mask := Shift_Left
        ((Shift_Left (1, Length) - 1), Start_Bit_Pos - Length + 1);
      Data_Aux := Shift_Left
        (Data_Aux, Start_Bit_Pos - Length + 1);
      Data_Aux := Data_Aux and Mask;
      Register_Value := Register_Value and not Mask;
      Register_Value := Register_Value or Data_Aux;

      HMC5883L_Write_Byte_At_Register (Device, Reg_Addr, Register_Value);
   end HMC5883L_Write_Bits_At_Register;

   --------------------------------------
   -- Fuse_Low_And_High_Register_Parts --
   --------------------------------------

   function Fuse_Low_And_High_Register_Parts
     (High : Byte;
      Low  : Byte) return Integer_16
   is
      ---------------------
      -- Uint16_To_Int16 --
      ---------------------

      function Uint16_To_Int16 is new Ada.Unchecked_Conversion
        (Unsigned_16, Integer_16);

      Register : Unsigned_16;
   begin
      Register := Shift_Left (Unsigned_16 (High), 8);
      Register := Register or Unsigned_16 (Low);

      return Uint16_To_Int16 (Register);
   end Fuse_Low_And_High_Register_Parts;

end HMC5883L;
