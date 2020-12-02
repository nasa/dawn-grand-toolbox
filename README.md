# dawn-grand-toolbox

Software to access and process data acquired by the NASA Dawn mission's Gamma-Ray and Neutron Detector (GRaND). The software consists of routines written in the Interactive Data Language (IDL), compatible with IDL version 8.6.1+. The GRaND archive consists entirely of character and binary tables. Two routines are provided to ingest the data for processing. These are found in the grd4-IDL subdirectory:

## grd4_read_character_table
Function that reads character (ASCII) tables, including most raw files and all calibrated and derived data files.

## grd4_read_binary_table
Function that reads binary data found in raw data files that contain event-mode gamma-ray and neutron data (-EMG and -EMN files).

The routines parse the label, extracting metadata useful for analysis, and read the data. For time series data files (raw and calibrated), the metadata includes mission phase and target infomation instrument settings (STATE, TELREADOUT, and TELSOH) and observing conditions.
