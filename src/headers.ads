with Interfaces; use Interfaces;

package Headers is
   
   type Byte is range 0 .. 16#ff#;

   type String_Access is access String;

   type File_Signature is
      record
         Magic_Number : Unsigned_64;
         Extension    : String_Access;
         Offset       : Unsigned_64;
         Bits         : Natural;
         Description  : String_Access;
      end record;

   Unknown_Header : exception;

   type Signatures is array (Positive range <>) of File_Signature;

   type Magic_Bytes is array (Positive range <>) of Byte;

   Zlib_No_Compression      : constant Unsigned_64 := 16#78_01#;
   Zlib_Default_Compression : constant Unsigned_64 := 16#78_9C#;
   Zlib_Best_Compression    : constant Unsigned_64 := 16#78_DA#;

   Dos_Executable          : constant Unsigned_64 := 16#4D_5A#;
   Tar_Z_Lzw               : constant Unsigned_64 := 16#1f_9d#; --  Compressed with Lempel-Ziv-Welch algorithm
   Tar_Z_Lzh               : constant Unsigned_64 := 16#1f_0a#; --  Compressed with LZH
   Bz2                     : constant Unsigned_64 := 16#42_5A_68#;
   Webm                    : constant Unsigned_64 := 16#1A_45_Df_A3#;
   Psd                     : constant Unsigned_64 := 16#38_42_50_53#;
   Flif                    : constant Unsigned_64 := 16#46_4c_49_46#;
   Ogg                     : constant Unsigned_64 := 16#4F_67_67_53#;
   Rpm                     : constant Unsigned_64 := 16#ed_ab_ee_db#;
   Kindle_Updater          : constant Unsigned_64 := 16#53_50_30_31#;
   Palm_Calendar_Archive   : constant Unsigned_64 := 16#be_ba_fe_ca#;
   Palm_To_Do_Archive      : constant Unsigned_64 := 16#00_01_42_44#;
   Palm_Calendar_Archive_2 : constant Unsigned_64 := 16#00_01_44_54#;
   Palm_Data_File          : constant Unsigned_64 := 16#00_01_00_00#;
   Ico                     : constant Unsigned_64 := 16#00_00_01_00#;
   Tiff_Little_Endian      : constant Unsigned_64 := 16#49_49_2A_00#;
   Tiff_Little_Big_Endian  : constant Unsigned_64 := 16#4D_4D_00_2A#;
   Gif_87a                 : constant Unsigned_64 := 16#47_49_46_38_37_61#;
   Gif_89a                 : constant Unsigned_64 := 16#47_49_46_38_39_61#;
   Rar_1_50                : constant Unsigned_64 := 16#52_61_72_21_1a_07_00#;
   Ms_Office               : constant Unsigned_64 := 16#d0_cf_11_e0_a1_b1_1a_e1#;
   Rar_5                   : constant Unsigned_64 := 16#52_61_72_21_1a_07_01_00#;
   Tar_Ustar_00            : constant Unsigned_64 := 16#75_73_74_61_72_00_30_30#;
   Tar_Ustar               : constant Unsigned_64 := 16#75_73_74_61_72_20_20_00#;
   Png                     : constant Unsigned_64 := 16#89_50_4e_47_0d_0a_1a_0a#;

   -- TODO
   -- Cr2           49_49_2A_00_10_00_00_00_43_52
   -- Backmike_Disk 42_41_43_4b_4d_49_4b_45_44_49_53_4b

   All_File_Signatures : constant Signatures
     := (
         (Magic_Number => Dos_Executable,
          Extension => new String'("exe"),
          Bits => 16,
          Offset => 0,
          Description => new String'("MS Dos Executable")),

         (Magic_Number => Tar_Z_Lzw,
          Extension    => new String'("tar"),
          Bits         => 0,
          Offset       => 0,
          Description  => new String'("Tar LZW archive")),

         (Magic_Number => Tar_Z_Lzh,
          Extension    => new String'("tar"),
          Bits         => 0,
          Offset       => 0,
          Description  => new String'("Tar LZH archive")),

         (Magic_Number => Bz2,
          Extension    => new String'("bz"),
          Bits         => 24,
          Offset       => 0,
          Description  => new String'("bunzip archive")),

         (Magic_Number => Webm,
          Extension    => new String'("webm"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("webm video file")),

         (Magic_Number => Psd,
          Extension    => new String'("psd"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("adobe photoshop document")),

         (Magic_Number => Flif,
          Extension    => new String'("flif"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("free lossless image format")),

         (Magic_Number => Ogg,
          Extension    => new String'("ogg"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("Ogg audio")),

         (Magic_Number => Rpm,
          Extension    => new String'("rpm"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("redhat package")),

         (Magic_Number => Kindle_Updater,
          Extension    => new String'("bin"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("kindle updater")),

         (Magic_Number => Palm_Calendar_Archive,
          Extension    => new String'("dba"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("palm calendar archive")),

         (Magic_Number => Palm_To_Do_Archive,
          Extension    => new String'("dba"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("palm todo archive")),

         (Magic_Number => Palm_Calendar_Archive_2,
          Extension    => new String'("tda"),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("palm calendar archive")),

         (Magic_Number => Palm_Data_File,
          Extension    => new String'(""),
          Bits         => 32,
          Offset       => 0,
          Description  => new String'("palm desktop data file (ms access format)")),

         (Magic_Number => Gif_87a,
          Extension    => new String'("gif"),
          Bits         => 48,
          Offset       => 0,
          Description  => new String'("gif picture (87a)")),

         (Magic_Number => Gif_89a,
          Extension    => new String'("gif"),
          Bits         => 48,
          Offset       => 0,
          Description  => new String'("gif picture (89a)")),

         (Magic_Number => Png,
          Extension    => new String'("png"),
          Bits         => 0,
          Offset       => 0,
          Description  => new String'("PNG picture")),

         (Magic_Number => Zlib_No_Compression,
          Extension    => new String'("zlib"),
          Bits               => 16,
          Offset       => 0,
          Description  => new String'("zlib no compression")),

         (Magic_Number => Zlib_Default_Compression,
          Extension    => new String'("zlib"),
          Bits               => 16,
          Offset       => 0,
          Description  => new String'("zlib default compression")),

         (Magic_Number => Zlib_Best_Compression,
          Extension    => new String'("zlib"),
          Bits               => 16,
          Offset       => 0,
          Description  => new String'("zlib best compression"))
        );

   procedure Print_File_Info (F : File_Signature);
end Headers;
