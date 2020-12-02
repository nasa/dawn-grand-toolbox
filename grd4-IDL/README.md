# grd4-IDL

IDL routines for the NASA Dawn GRaND PDS4 archive.

## grd4_read_character_table

### Description:
Reads a file containing character (ASCII) tables in the Dawn/GRaND PDS4 archive.

### Inputs:
* `label_file`   - (keyword) name of the XML label file, e.g. '<prefix>.xml' 
;                  If not set, then dialog_pickfile is used to interactively 
;                  select the file.
* `directory`    - (keyword) if set, gives the directory containing the file.

### Outputs:
* Structure containing selected metadata from the XML file and a structure array
containing the data records. The structure always contains a flag which is
set to true if successful and false if there was an error.

### Examples: 
```
IDL> directory='.\dawn-grand-ceres\data_raw\01_CERES_SCIENCE_APPROACH\GRD-L1A-150313-150319_150625\_aux\'
IDL> label_file='GRD-L1A-150313-150319_150625-SOH-SCL.xml'
IDL> g=grd4_read_character_table(lab=label_file,directory=directory)
IDL> help, g.records, /str
** Structure <26f1f4a0>, 4 tags, length=120, data length=116, refs=2:
   SCET_UTC        STRING    '2015-03-13T03:10:35'
   SCLK            LONG         479488302
   PSC_SOH         LONG                 1
   SCALER_SOH      LONG      Array[23]
IDL> print, g.records[0:3], format=g.format  ; fortran style format
2015-03-13T03:10:35  479488302     1     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0  3876  2000  2800
2015-03-13T03:11:35  479488362     2     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0  3876  2000  2800
2015-03-13T03:11:35  479488362     3     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0  3876  2000  2800
2015-03-13T03:12:35  479488422     4     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0  3876  2000  2800
```

```
IDL> directory='.\dawn-grand-vesta\data_derived\'
IDL> label_file='GRD_IRON_CORRECTED_COUNTS_MAP.xml'
IDL> g=grd4_read_character_table(lab=label_file,dir=directory)
IDL> help, g.records, /str
** Structure <e4f3440>, 8 tags, length=64, data length=60, refs=2:
   PIXEL_INDEX     LONG                 0
   MIN_LAT         DOUBLE          -90.000000
   MAX_LAT         DOUBLE          -89.500000
   DELTA_LAT       DOUBLE          0.50000000
   MIN_LON         DOUBLE          -30.000000
   MAX_LON         DOUBLE           90.000000
   DELTA_LON       DOUBLE           120.00000
   FE_COUNTING_RATE
                   DOUBLE         0.069460000
IDL> for i=0,3 do print, g.records[i], format=g.formatc  ; C style format
         0  -90.0  -89.5    0.5  -30.0   90.0  120.0   6.946e-002
         1  -90.0  -89.5    0.5   90.0 -150.0  120.0   6.918e-002
         2  -90.0  -89.5    0.5 -150.0  -30.0  120.0   6.917e-002
         3  -89.5  -89.0    0.5  -30.0   -6.0   24.0   6.937e-002
```

## grd4_read_binary_table

### Description:
Reads a file containing binary data table in the Dawn/GRaND PDS4 archive. 

### Inputs:
* `label_file`   - (keyword) name of the XML label file, e.g. '<prefix>.xml' 
;                  If not set, then dialog_pickfile is used to interactively 
;                  select the file (EMG and EMN labels are filtered).
* `directory`    - (keyword) if set, gives the directory containing the file.

### Outputs:
* Structure containing selected metadata from the XML file and a structure array
containing the data records. The structure always contains a flag which is
set to true if successful and false if there was an error.

### Example: 
```
IDL> directory='.\dawn-grand-vesta\data_raw\5_VESTA_TRANSFER_TO_LAMO\GRD-L1A-111201-111208_130628\gamma\'
IDL> label_file='GRD-L1A-111201-111208_130628-EMG.xml'
IDL> g=grd4_read_binary_table(lab=label_file,directory=directory)
IDL> help, g.records, /str
** Structure <ef4d250>, 6 tags, length=19496, data length=19492, refs=2:
   SCET_UTC        STRING    '2011-12-01T00:00:20 '
   SCLK            ULONG        375969687
   SCALER_SCI      ULONG     Array[23]
   ID_CZT          BYTE      Array[3876]
   CH_CZT          UINT      Array[3876]
   CH_BGO          UINT      Array[3876]
```

