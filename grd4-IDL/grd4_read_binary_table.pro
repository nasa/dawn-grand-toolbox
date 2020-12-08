function grd4_read_binary_table, label_file=label_file, directory=directory
;
; Purpose: Reads a file containing binary data table in the Dawn/GRaND PDS4 archive. 
;
; Inputs:
;   label_file   - (keyword) name of the XML label file, e.g. '<prefix>.xml' 
;                  If not set, then dialog_pickfile is used to interactively 
;                  select the file (EMG and EMN labels are filtered).
;   directory    - (keyword) if set, gives the directory containing the file.
;
; Outputs:
;   Structure containing selected metadata from the XML file and a structure array
;   containing the data records. The structure always contains a flag which is
;   set to true if successful and false if there was an error.
;   
; Limitations:
;   This is not a general IDL-based PDS4 reader. The current version only 
;   supports reading of binary tables in the GRaND archive, including the
;   event mode neutron (-EMN.dat) and gamma-ray data (-EMG.dat) files.
;
; IDL version: Valid for IDL 8.6.1+
;
; Dependencies: 
;   Uses the xml_parse() function introduced in IDL 8.6.1
;
; Examples:
;   directory='.\dawn-grand-vesta\data_raw\5_VESTA_TRANSFER_TO_LAMO\GRD-L1A-111201-111208_130628\gamma\'
;   label_file='GRD-L1A-111201-111208_130628-EMG.xml'
;   g=grd4_read_binary_table(lab=label_file,directory=directory)
;
; Version:
;   This function was developed to support the PDS3 -> PDS4 migration of the 
;   NASA Dawn GRaND archive sponsored by the Planetary Data System Small Bodies Node.
;   -- 19-Jun-2020 T.H. Prettyman (THP) - V1.0 Created - limited to binary tables
;   -- 22-Jun-2020 NY added file_search() to append a path separator when missing
;   -- 28-Jul-2020 THP added state, telreadout & telsoh to output structure
;   -- 1-Dec-2020 THP updated to include NucSpec features 
;   -- 8-Dec-2020 THP updated output structure to echo include the data file
;


if keyword_set(label_file) then begin
  file=label_file & if keyword_set(directory) then file=file_search(directory,/mark_directory)+label_file
endif else begin
  path='.'+path_sep() & if keyword_set(directory) then path=directory
  file=dialog_pickfile(filter=['*EMN*.xml','*EMG*.xml'],path=path)
  if file eq '' then begin
    print, 'grd4_read_character_table: No file selected. '
    result={flag:!false}  
    goto, EXIT
  endif
endelse

if not file_test(file[0]) then begin
  print, 'grd4_read_binary_table: Label file not found: '
  print, strtrim(string(file),2)
  result={flag:!false}
  goto, EXIT
endif

xml=xml_parse(file[0])
data_file_name=xml['Product_Observational','File_Area_Observational','File','file_name']
ascii=!false & if data_file_name.contains('.tab',/fold_case) then ascii=!true

if ascii then begin
  print, 'grd4_read_binary_table: Only binary tables are treated by this function. For character tables, use grd4_read_character_table().'
  result={flag:!false}
  goto, EXIT
endif

data_file=file.replace('.xml','.dat',/fold_case)
if not file_test(data_file) then begin
  print, 'grd4_read_binary_table: Data file not found - ', strtrim(string(data_file),2)
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

fields=xml['Product_Observational','File_Area_Observational','Table_Binary','Record_Binary','Field_Binary']
tag=strarr(fields.length) & data_type=strarr(fields.length) 
defvals=strarr(fields.length)
for i=0L,fields.length-1 do begin & $
  keys=fields[i].keys() & keys=keys.ToArray(type=string) & $
  values=fields[i].values() & $
  ik=where(keys.contains('name')) & tag[i]=values[ik[0]]            & $
  ik=where(keys.contains('data_type')) & data_type[i]=values[ik[0]] & $
  case data_type[i] of & $
    'ASCII_Date_Time_YMD':   defvals[i] = "'yyyy-mm-ddThr:mn:sc '"  & $
    'UnsignedMSB4':          defvals[i] = 'ulong(0)'                & $
    'UnsignedMSB2':          defvals[i] = 'uint(0)'                 & $
    'UnsignedByte':          defvals[i] = 'byte(0)'                 & $
  endcase & $
endfor

cs='' & qt="'"
for i=0L,fields.length-2 do cs += qt+tag[i]+qt+','+defvals[i]+',' 
cs += qt+tag[fields.length-1]+qt+','+defvals[fields.length-1]

gfb=xml['Product_Observational','File_Area_Observational','Table_Binary','Record_Binary','Group_Field_Binary']
for i=0L,gfb.length-1 do begin
  keys=gfb[i].keys() & keys=keys.ToArray(type=string) 
  values=gfb[i].values() & $
  ik=where(keys.contains('name')) & tag=values[ik[0]]            
  ik=where(keys.contains('repetitions')) & rep_str=values[ik[0]] 
    ik=where(keys.contains('Field_Binary')) & fb=values[ik[0]]     
    keys=fb.keys() & keys=keys.ToArray(type=string)
    id=where(keys.contains('data_type'))
    values=fb.values() & data_type=values[id[0]]
    case data_type of & $
      'UnsignedMSB4':          defvals = 'ulonarr'                 & $
      'UnsignedMSB2':          defvals = 'uintarr'                 & $
      'UnsignedByte':          defvals = 'bytarr'                  & $
    endcase 
  cs += ','+qt+tag+qt+','+defvals+'('+rep_str+')'
endfor

void=execute('record=create_struct('+cs+')')

records=replicate(record,nr)

openu, lun, data_file, /get_lun, /swap_if_little_endian  ; the data are recorded as MSB (Big Endian) 
for i=0L,nr-1 do begin 
  readu, lun, record
  records[i]=record 
endfor
free_lun, lun

result={flag:!true, data_file:data_file, target_name:target_name}
if not mp_test then result=create_struct(result, 'mission_phase_name', mission_phase_name, $
                                                 'mission_phase_identifier', mission_phase_identifier, $
                                                 'sclk_start', sclk_start, $
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

result=create_struct(result, 'records', records)                                                 

EXIT:
return, result
end
