function grd4_C_to_fort_format_code, cform
;
; Purpose: Converts a selected set of C format codes to thier
;          fortran equivalent.
;
; Inputs:
;    cform - C format code string - select from the following:
;              %wS, %wD, %w.dE, %w.dF (case insensitive)
;
; Output:
;    fform - The equivalent fortran code, i.e. 
;              Aw, Iw, Ew.d, Fw.d 
; 
; Limitations: Developed to treat combinations of format codes
;              found in the NASA PDS Dawn/GRaND archive. Not
;              a general treatment of format codes.
;              ** no error checking **
; 
; Example:
;    print, grd4_C_to_fort_format_code('%12.1E')  ; equivalent to 'e12.1'
;
; Dependencies: none 
;
; Version:
;   This function was developed to support the PDS3 -> PDS4 migration of the 
;   NASA Dawn GRaND archive sponsored by the Planetary Data System Small Bodies Node.
;   -- 18-Jun-2020 THP - V1.0 Created 
;

c=cform.extract('s|d|e|f',/fold_case) & c=c.ToLower()

if cform.matches('e|f',/fold_case) then w=cform.extract('[0-9]+.[0-9]+')

if cform.matches('s|d',/fold_case) then w=cform.extract('[0-9]+')

case c of 
  's':f='a'
  'd':f='i'
  'e':f='e'
  'f':f='f'
endcase

return, f+w
end
