function grd4_read_character_table, label_file=label_file, directory=directory
;
; Purpose: Reads a file containing character (ASCII) tables in the Dawn/GRaND PDS4 archive.
;
; Inputs:
;   label_file   - (keyword) name of the XML label file, e.g. '<prefix>.xml' 
;                  If not set, then dialog_pickfile is used to interactively 
;                  select the file.
;   directory    - (keyword) if set, gives the directory containing the file.
;
; Outputs:
;   Structure containing selected metadata from the XML file and a structure array
;   containing the data records. The structure always contains a flag which is
;   set to true if successful and false if there was an error.
;   
; Limitations:
;   This is not a general IDL-based PDS4 reader. The current version only 
;   supports reading of character (ASCII) tables in the GRaND archive.
;   **To accommodate tables with repetitions, the C printf-style format
;     codes used in PDS4 are converted to fortran format. IDL does not
;     allow repetition directly via C-formatting; however, mix and match
;     C and fortran formatting is permitted. This version builds the
;     mixed format string and provides both formats in the output 
;     structure. Note that the following produce equivalent results when
;     g is the ouput structure for a file containing a vector:
;
;        print, g.records[0:9], format=g.format  ; fortran style
;        for i=0,9 do print, g.records[i], format=g.formatc  ; C style
;    
;     In this case, records do not transfer properly for C style 
;     formatting:
;
;        print, g.records[0:9], format=g.formatc  ; C style
;
; IDL version: Valid for IDL 8.6.1+
;
; Dependencies: 
;   Uses the xml_parse() function introduced in IDL 8.6.1
;   grd4_C_to_fort_format_code() function included in this package
;
; Examples:
;   directory='.\dawn-grand-ceres\data_raw\01_CERES_SCIENCE_APPROACH\GRD-L1A-150313-150319_150625\_aux'
;   label_file='GRD-L1A-150313-150319_150625-SOH-SCL.xml'
;   g=grd4_read_character_table(lab=label_file,directory=directory)
;   print, g.records[0:9], format=g.format  ; fortran style format
;
;   directory='.\dawn-grand-vesta\data_derived\'
;   label_file='GRD_IRON_CORRECTED_COUNTS_MAP.xml'
;   g=grd4_read_character_table(lab=label_file,dir=directory)
;   for i=0,3 do print, g.records[i], format=g.formatc  ; C style format
;
; Version:
;   This function was developed to support the PDS3 -> PDS4 migration of the 
;   NASA Dawn GRaND archive sponsored by the Planetary Data System Small Bodies Node.
;   -- 17-Jun-2020 T.H. Prettyman (THP) - V1.0 Created - limited to ASCII tables
;   -- 22-Jun-2020 THP added dialog_pickfile() & format string to returned structure
;   -- 22-Jun-2020 Naoyuki Yamashita (NY) added file_search() to append a path separator when missing
;   -- 24-Jun-2020 THP added mixed C-fortran format string to output structure
;   -- 26-Jun-2020 THP added more obervational variables to output structure
;   -- 28-Jul-2020 THP added state, telreadout & telsoh to output structure
;   -- 22-Oct-2020 THP updated to account for nil S/C sclk_start/stop in some files;
;                      allows for single field character (affects one file type,
;                      calibrated Mars BGO spectra)
;

if keyword_set(label_file) then begin
  file=label_file & if keyword_set(directory) then file=file_search(directory,/mark_directory)+label_file
endif else begin
  path='.'+path_sep() & if keyword_set(directory) then path=directory
  file=dialog_pickfile(filter='*.xml',path=path)
  if file eq '' then begin
    print, 'grd4_read_character_table: No file selected. '
    result={flag:!false}  
    goto, EXIT
  endif
endelse

if not file_test(file[0]) then begin
   print, 'grd4_read_character_table: Label file not found: '
   print, strtrim(string(file),2)
   result={flag:!false}  
   goto, EXIT
 endif

xml=xml_parse(file[0])
data_file_name=xml['Product_Observational','File_Area_Observational','File','file_name']
binary=!false & if data_file_name.contains('.dat',/fold_case) then binary=!true

if binary then begin
  print, 'grd4_read_character_table: Only character tables are treated by this function. For binary tables, use grd4_read_binary_table().'
  result={flag:!false}
  goto, EXIT
endif

data_file=file.replace('.xml','.tab',/fold_case)
if binary then data_file=file.replace('.xml','.dat',/fold_case)
if not file_test(data_file) then begin
  print, 'grd4_read_character_table: Data file not found - ', strtrim(string(data_file),2)
  result={flag:!false}
  goto, EXIT
endif

records=xml['Product_Observational','File_Area_Observational','File','records'] & nr=0L & reads, records, nr
target_name=''
if typename(xml['Product_Observational','Observation_Area','Target_Identification']) ne 'LIST' then $
   target_name=xml['Product_Observational','Observation_Area','Target_Identification','name']
mp_test=!false 
if typename(xml['Product_Observational','Observation_Area','Discipline_Area','msn:Mission_Information']) eq 'LIST' then mp_test=!true
mission_phase_name='' & mission_phase_identifier='' & sclk_start=0L & sclk_stop=0L
if not mp_test then begin
  keys=xml['Product_Observational','Observation_Area','Discipline_Area','msn:Mission_Information'].keys() & keys=keys.ToArray(type=string)
  if total(keys.contains('msn:mission_phase_name')) then $
  mission_phase_name=xml['Product_Observational','Observation_Area','Discipline_Area','msn:Mission_Information','msn:mission_phase_name']
  if total(keys.contains('msn:mission_phase_identifier')) then $
  mission_phase_identifier=xml['Product_Observational','Observation_Area','Discipline_Area','msn:Mission_Information','msn:mission_phase_identifier']
  if total(keys.contains('msn:spacecraft_clock_start')) then begin 
    ss=xml['Product_Observational','Observation_Area','Discipline_Area','msn:Mission_Information','msn:spacecraft_clock_start']
    reads, ss, sclk_start
  endif
  if total(keys.contains('msn:spacecraft_clock_stop')) then begin
    ss=xml['Product_Observational','Observation_Area','Discipline_Area','msn:Mission_Information','msn:spacecraft_clock_stop']
    reads, ss, sclk_stop
  endif
endif

qt="'" & dqt='"'
fields=xml['Product_Observational','File_Area_Observational','Table_Character','Record_Character','Field_Character']
if typename(fields) eq 'ORDEREDHASH' then begin
  tag='' & data_type='' & format='' & formatc='' & defvals=''
  keys=fields.keys() & keys=keys.ToArray(type=string) 
  values=fields.values() 
  ik=where(keys.contains('name')) & tag=values[ik[0]]            
  ik=where(keys.contains('data_type')) & data_type=values[ik[0]] 
  ik=where(keys.contains('format')) & format=grd4_C_to_fort_format_code(values[ik[0]]) 
  ik=where(keys.contains('format')) & formatc=values[ik[0]] 
  case data_type of & $
    'ASCII_Date_Time_YMD':   defvals = "''"   & $
    'ASCII_String':          defvals = "''"   & $
    'ASCII_Integer':         defvals = '0L'   & $
    'ASCII_Real':            defvals = '0d'   & $
  endcase 

  cs=qt+tag+qt+','+defvals
  form=format
  formc=dqt+formatc+dqt

endif else begin
  tag=strarr(fields.length) & data_type=strarr(fields.length) & format=strarr(fields.length) & formatc=strarr(fields.length)
  defvals=strarr(fields.length)
  for i=0L,fields.length-1 do begin & $
    keys=fields[i].keys() & keys=keys.ToArray(type=string) & $
    values=fields[i].values() & $
    ik=where(keys.contains('name')) & tag[i]=values[ik[0]]            & $
    ik=where(keys.contains('data_type')) & data_type[i]=values[ik[0]] & $
    ik=where(keys.contains('format')) & format[i]=grd4_C_to_fort_format_code(values[ik[0]]) & $
    ik=where(keys.contains('format')) & formatc[i]=values[ik[0]] & $
    case data_type[i] of & $
      'ASCII_Date_Time_YMD':   defvals[i] = "''"   & $
      'ASCII_String':          defvals[i] = "''"   & $
      'ASCII_Integer':         defvals[i] = '0L'   & $
      'ASCII_Real':            defvals[i] = '0d'   & $
    endcase & $
  endfor
  
  cs='' & for i=0L,fields.length-2 do cs += qt+tag[i]+qt+','+defvals[i]+',' 
  cs += qt+tag[fields.length-1]+qt+','+defvals[fields.length-1]

  form=format[0]+',' & for i=1L,fields.length-2 do form += format[i]+',' & form += format[fields.length-1] 
  formc=dqt & for i=0L,fields.length-1 do formc += formatc[i] & formc += dqt
endelse

rec=xml['Product_Observational','File_Area_Observational','Table_Character','Record_Character']
rec_keys=rec.keys() & rec_keys=rec_keys.ToArray()
id=where(rec_keys.contains('Group_Field_Character'),count)
if count eq 1 then begin 
  gfc_tag=xml['Product_Observational','File_Area_Observational','Table_Character','Record_Character','Group_Field_Character','name']
  gfc_rep_str=xml['Product_Observational','File_Area_Observational','Table_Character','Record_Character','Group_Field_Character','repetitions']
  gfc_data_type=xml['Product_Observational','File_Area_Observational','Table_Character','Record_Character','Group_Field_Character','Field_Character','data_type']
  temp=xml['Product_Observational','File_Area_Observational','Table_Character','Record_Character','Group_Field_Character','Field_Character','field_format']
  gfc_format=grd4_C_to_fort_format_code(temp)
  gfc_format1=temp
  case gfc_data_type of & $
    'ASCII_Date_Time_YMD':   gfc_defvals = 'strarr'   & $
    'ASCII_String':          gfc_defvals = 'strarr'   & $
    'ASCII_Integer':         gfc_defvals = 'lonarr'   & $
    'ASCII_Real':            gfc_defvals = 'dblarr'   & $
  endcase 
  cs += ','+qt+gfc_tag+qt+','+gfc_defvals+'('+gfc_rep_str+')'
  form += ','+gfc_rep_str+gfc_format
  formc += ','+gfc_rep_str+'(%'+dqt+gfc_format1+dqt+')'
endif

void=execute('record=create_struct('+cs+')')

records=replicate(record,nr)

form='('+form+')'
formc='(%'+formc+')'

openr, lun, data_file, /get_lun
for i=0L,nr-1 do begin 
  readf, lun, record, format=form
  records[i]=record 
endfor
free_lun, lun

result={flag:!true, target_name:target_name}
if not mp_test then result=create_struct(result, 'mission_phase_name', mission_phase_name, $
                                                 'mission_phase_identifier', mission_phase_identifier)
if sclk_start ne 0 then result=create_struct(result, 'sclk_start', sclk_start, $
                                                     'sclk_stop', sclk_stop)

obs=xml['Product_Observational','Observation_Area','Discipline_Area']  & temp=obs.keys() & keys=temp.ToArray(type=string)

id=where(keys.contains('nucspec:NucSpec_Observation_Properties'),count)
if count ne 0 then begin

  nuc=obs['nucspec:NucSpec_Observation_Properties','nucspec:Instrument_Settings','nucspec:State_Table','nucspec:State_Table_Entry']
  if typename(nuc) eq 'ORDEREDHASH' then begin
    state=0L & sclk_start=0L & sclk_stop=0L
    y=nuc['nucspec:state_index']
    if typename(y) eq 'STRING' then begin
      temp=0L & reads, y, temp 
      state=temp 
    endif
    y=nuc['nucspec:State_Time','nucspec:Time_Range_SCLK','nucspec:sclk_start_time'] & temp=0L & reads, y, temp & sclk_start=temp
    y=nuc['nucspec:State_Time','nucspec:Time_Range_SCLK','nucspec:sclk_stop_time'] & temp=0L & reads, y, temp & sclk_stop=temp
  endif else begin
    state=lonarr(nuc.length) & sclk_start=lonarr(nuc.length) & sclk_stop=lonarr(nuc.length)
    for i=0L,nuc.length-1 do begin
      x=nuc[i] & y=x['nucspec:state_index']
      if typename(y) eq 'STRING' then begin
        temp=0L & reads, y, temp 
        state[i]=temp 
      endif
      y=x['nucspec:State_Time','nucspec:Time_Range_SCLK','nucspec:sclk_start_time'] & temp=0L & reads, y, temp & sclk_start[i]=temp
      y=x['nucspec:State_Time','nucspec:Time_Range_SCLK','nucspec:sclk_stop_time'] & temp=0L & reads, y, temp & sclk_stop[i]=temp
    endfor
  endelse
  sclk=records.sclk & state_vec=lonarr(sclk.length)
  for i=0L,state.length-1 do begin
    ix=where(sclk ge sclk_start[i] and sclk le sclk_stop[i],cx)
    if cx ne 0 then state_vec[ix]=state[i]
  endfor
  result=create_struct(result, 'state', state_vec)


  test=obs['nucspec:NucSpec_Observation_Properties'] 
  if typename(test) eq 'ORDEREDHASH' then begin
    nuc=obs['nucspec:NucSpec_Observation_Properties','nucspec:Observing_Conditions']
    obscon={}
    for i=0L,nuc.length-1 do begin
      kk=nuc[i].keys() & vv=nuc[i].values()
      condition_type=vv[0] & v1=vv[1] ; order per dictionary
      case condition_type.ToLower() of
        'sep': var_name='sep'
        'solar flare': var_name='sflare'
        'gamma-ray burst' : var_name='grb'
        'data glitch' : var_name='glitch'
      else: goto, JUMP1
      endcase
      sclk0=0L & sclk1=0L & desc='' & flag=boolarr(nr) & pt=1L
      if typename(v1) eq 'ORDEREDHASH' then begin
        kk1=v1.keys() & vv1=v1.values()
        if vv1[0] eq 'true' then begin
          desc=[desc,vv1[1]]
          vv2=vv1[2].values()
          temp=0L & reads, vv2[0], temp & sclk0=[sclk0,temp] 
          temp=0L & reads, vv2[1], temp & sclk1=[sclk1,temp] 
          ixx=where(sclk ge sclk0[pt] and sclk le sclk1[pt], countxx) & if countxx gt 0 then flag[ixx]=1
          pt++
        endif
      endif else begin
        for j=0L,v1.length-1 do begin
          kk1=v1[j].keys() & vv1=v1[j].values()
          if vv1[0] eq 'true' then begin
            desc=[desc,vv1[1]]
            vv2=vv1[2].values()
            temp=0L & reads, vv2[0], temp & sclk0=[sclk0,temp] 
            temp=0L & reads, vv2[1], temp & sclk1=[sclk1,temp] 
            ixx=where(sclk ge sclk0[pt] and sclk le sclk1[pt], countxx) & if countxx gt 0 then flag[ixx]=1
            pt++
          endif 
        endfor
      endelse
      if sclk0.length eq 1 then begin
        obscon=create_struct(obscon, 'active_'+var_name, boolean(0), 'flag_'+var_name, flag)
      endif else begin
        sclk0=sclk0[1:-1] & sclk1=sclk1[1:-1] & desc=desc[1:-1]
        obscon=create_struct(obscon, 'active_'+var_name, boolean(1), 'sclk_start_'+var_name, sclk0, 'sclk_stop_'+var_name, sclk1, 'desc_'+var_name, desc, 'flag_'+var_name, flag)
      endelse
      JUMP1:
    endfor
  endif
endif

id=where(keys.contains('dawn:grand'),count)
if count gt 0 then begin
  grd=obs['dawn:grand','dawn:TELREADOUT_List','dawn:TELREADOUT']
  if typename(grd) eq 'ORDEREDHASH' then begin
    telreadout=0L & sclk_start=0L & sclk_stop=0L
    y=grd['dawn:grand_interval'] & v=y.values() & vv=v.ToArray(type=string)
    iy=where(vv.contains('missing'), cy) 
    if cy eq 0 then begin
      temp=0L & reads, vv[1], temp
      telreadout=temp 
    endif
    y=grd['dawn:grand_start_sclk'] & temp=0L & reads, y, temp & sclk_start=temp
    y=grd['dawn:grand_stop_sclk'] & temp=0L & reads, y, temp & sclk_stop=temp
  endif else begin  
    telreadout=lonarr(grd.length) & sclk_start=lonarr(grd.length) & sclk_stop=lonarr(grd.length)
    for i=0L,grd.length-1 do begin
      x=grd[i] & y=x['dawn:grand_interval'] & v=y.values() & vv=v.ToArray(type=string)
      iy=where(vv.contains('missing'), cy) 
      if cy eq 0 then begin
        temp=0L & reads, vv[1], temp
        telreadout[i]=temp 
      endif
      y=x['dawn:grand_start_sclk'] & temp=0L & reads, y, temp & sclk_start[i]=temp
      y=x['dawn:grand_stop_sclk'] & temp=0L & reads, y, temp & sclk_stop[i]=temp
    endfor
  endelse  
  grd=obs['dawn:grand','dawn:TELSOH_List','dawn:TELSOH']
  if typename(grd) eq 'ORDEREDHASH' then begin
    telsoh=0L
    y=grd['dawn:grand_interval'] & v=y.values() & vv=v.ToArray(type=string)
    iy=where(vv.contains('missing'), cy) 
    if cy eq 0 then begin
      temp=0L & reads, vv[1], temp
      telsoh=temp 
    endif
  endif else begin  
    telsoh=lonarr(grd.length) 
    for i=0L,grd.length-1 do begin
      x=grd[i] & y=x['dawn:grand_interval'] & v=y.values() & vv=v.ToArray(type=string)
      iy=where(vv.contains('missing'), cy) 
      if cy eq 0 then begin
        temp=0L & reads, vv[1], temp
        telsoh[i]=temp 
      endif
    endfor
  endelse 
  sclk=records.sclk & telr=lonarr(sclk.length) & tels=lonarr(sclk.length)
  for i=0L,telreadout.length-1 do begin
    ix=where(sclk ge sclk_start[i] and sclk le sclk_stop[i],cx)
    if cx ne 0 then telr[ix]=telreadout[i]
    if cx ne 0 then tels[ix]=telsoh[i]
  endfor
  result=create_struct(result, 'telreadout', telr, 'telsoh', tels)
endif

if typename(obscon) eq 'ANONYMOUS' then result=create_struct(result, 'obscon', obscon)

result=create_struct(result, 'records', records, 'format', form, 'formatc', formc)                                                 

EXIT:
return, result
end