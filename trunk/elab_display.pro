;$Id$$

pro elab_display, the_val, the_list, FLAG=flag, TITLE=title, NO_PLOT=show , MAGELLAN=magellan $
              , POSITION=position, SMOOTH=smooth, SHOWBAR=showbar $
              , NO_NUMBERING=no_numbering, OUT_VAL=out_val $
              , _EXTRA=plot_keywords, NO_BACKGROUND=no_background $
              , ROT=rot, REFL=refl, min_v=min_pos , max_v=max_pos $
              , MAP_SAMPLING=map_samp, BAR_TITLE=bar_title, NUM_TYPE=num_type, NUM_LIST=num_list $
              , NOERASE=noerase, YTICKF_BAR=ytickf_bar, XSTYLE=xstyle, YSTYLE=ystyle, YTITLE=ytitle , XTITLE=xtitle $
              , ACT_PSYM=act_psym, ACT_SYMSIZE=act_symsize, NO_SMOOTH_VAL=no_smooth_val $
              , NUM_CHARTHICK=num_charthick, NUM_CHARSIZE=num_charsize, OUT_EL=out $
              , IN_EL=in, COL_INVERT=col_invert, LOG=log, INV=inv, SPOT_MAGNIFICATION=spot_mag, XBAR=xbar $
              , PARAM_FILE=param_file, ADSEC_SAVE=adsec_save, ADSEC_SHELL_SAVE=adsec_shell_save, SC_SAVE=sc_save, GR_SAVE=gr_save, SECOND_LIST=second_list
;+
; NAME:
;   DISPLAY
;
; PURPOSE:
;   Display an image based on the values sampled at the actuator locations (defined in the structure adsec).
;
; CATEGORY:
;   General graphics.
;
; CALLING SEQUENCE:
;       display, val, list, FLAG=flag, TITLE=title, NO_PLOT=show $
;              , POSITION=position, SMOOTH=smooth, SHOWBAR=showbar $
;              , NO_NUMBERING=no_numbering, OUT_VAL=out_val $
;              , _EXTRA=plot_keywords, NO_BACKGROUND=no_background $
;              , ROT=rot, REFL=refl, min_v=min_pos , max_v=max_pos $
;              , MAP_SAMPLING=map_samp, BAR_TITLE=bar_title, NUM_TYPE=num_type $
;              , NOERASE=noerase, YTICKF_BAR=ytickf_bar, XSTYLE=xstyle,YSTYLE=ystyle, YTITLE=ytitle $
;              , ACT_PSYM=act_psym, ACT_SYMSIZE=act_symsize, NO_SMOOTH_VAL=no_smooth_val $
;              , NUM_CHARTHICK=num_charthick, NUM_CHARSIZE=num_charsize, OUT_EL=out $
;              , IN_EL=in, COL_INVERT=col_invert, LOG=log, INV=inv
;
; INPUTS:
;   VAL:    Values assumed at the actuator locations defined by the input LIST.
;           The_val must have adsec.n_actuators elements or the same size of LIST.
;           In the first case the subset of the_val stated by LIST is considered.
;
;   LIST:   List of actuators used to sample the image.
;           If it is omitted LIST is set to adsec.true_act by default.
;
;
; KEYWORD PARAMETERS:
;
;   FLAG:   if not set shows a 2-d map of the mirror if set to 2 shows a shaeded surface
;
;   TITLE: sets the title of the plot
;
;   HIDEPLOT: If this keyword is set the plot is not shown (the image can be saved in the variable POSITION for future plotting)
;
;   POSITION: If set in the call equal to a named variable this variable returns the 2-d map
;
;   SMOOTH: Generates a smoothed version of the map.
;
;   SHOWBAR:    If set the color bar is shown
;
;   NO_NUMBERING: If set the actuator numbering is not shown
;
;   OUT_VAL: Allows to set the background color
;
;   _EXTRA: Allows to use the general keywords used with the plot routine to insert titles and other
;           features (see help on plot).
;
;   NO_BACKGROUND: Sets the background color to the minimum value
;
;
;   ROT: Allows to set the rotation of the actuator mapping (usually set to the values defined in the structure gr)
;
;   REFL: Allows to set the reflection along the x-axis of the actuator mapping (usually set to the values defined in the structure gr)
;
;   MIN_V: Sets the minimum value displaied
;
;   MAX_V: Sets the maximum value displaied
;
;   MAP_SAMPLING: Sets the number of pixels used in the map (it is set by default to 151 in the un-smoothed image and to 101 in the smoothed one)
;
;   SPOT_MAGNIFICATION: float scalar. 0 < spot_mag <= 1. Dimention of
;   spots in units of interactuator pitch. Default value is 0.25.
;
;   XBAR: set the initial point of the bar in x
;   axis. 0<xbar<1. Default 0.75.
;
;   NUM_LIST: set a index list of passed data which indicates the actuators that you want to have numberi displayed
;
; OUTPUTS:
;   No explicit outputs.
;
; COMMON BLOCKS:
;   adsec, gr.
;
; SIDE EFFECTS:
;   The currently selected display is affected.
;
; RESTRICTIONS:
;   None.
;
; PROCEDURE:
;
;
; MODIFICATION HISTORY:
;
;   GBZ several times.
;   Aug 2003: by A. Riccardi (AR).
;             More flexibility for the size of VAL has been introduced.
;             New default value for LIST (adsec.true_act)
;
;   2 Nov 2004: by D. Zanotti
;               adsec: in_radius, out_radius are changed in adsec_shell: in_radius, out_radius
;
;   20 May 2005: AR, SPOT_MAGNIFICATION keyword added
;
;   04 Aug 2005: by AR:
;                changed default output map size in case SMOOTH is not defined (from 151 to 255).
;                fixed bugs in actuator number and symbol display
;
;   28 April 2008 by AR, MX
;                 adsec_save keyword_added.
;
;   5 Nov 2008: by MX
;                 numlist keyword added. Set default xtitle and ytitle.
;
;-

if n_elements(XTITLE) eq 0 then xtitle='[mm]'
if n_elements(YTITLE) eq 0 then ytitle='[mm]'
if n_elements(param_file) gt 0 then restore, param_file else begin

    gr_save = {                 $
        num_type    : 0,         $
        theta_tv    : 0.0,       $
        x_reflect_tv: 0         $
    }

    if keyword_set(MAGELLAN) then begin
        adsec_shell_save = {        $
            out_radius : 425.350,    $ 
            in_radius  : 27.5000,    $
            ring_radius: [43.0439,73.3537,103.667,133.980,164.294,194.607,224.920,255.233,285.547,315.860,346.173,376.486,406.800], $ 
            n_act_ring: [9,15,21,27,33,39,45,51,57,63,69,75,81], $
            n_rings: 13 $
        }
    endif else begin
        adsec_shell_save = {         $
            out_radius : 455.500,    $ 
            in_radius  : 27.5000,    $
            ring_radius: [43.0439,73.3537,103.667,133.980,164.294,194.607,224.920,255.233,285.547,315.860,346.173,376.486,406.800,437.109], $ 
            n_act_ring: [9,15,21,27,33,39,45,51,57,63,69,75,81,87], $
            n_rings: 14 $
        }
    endelse

    act2mir_act = [ $
    504,505,506,507,585,586,587,588,360,361,362,363,429,430,431,432,509,510,511,590,591 $
    ,592,593,594,364,433,434,435,436,508,512,589,513,514,515,516,595,596,597,598,367,368 $
    ,369,370,437,438,439,440,246,247,248,249,304,305,306,307,298,299,300,301,302,303,365 $
    ,366,189,240,241,242,243,244,245,297,144,145,146,147,190,191,192,193,148,149,150,151 $
    ,194,195,196,197,72,105,106,107,108,109,110,111,47,48,49,73,74,75,76,77,0            $
    ,9,10,24,25,26,45,46,527,528,529,530,610,611,612,613,379,380,381,382,450,451         $
    ,452,453,523,524,525,604,605,606,607,608,378,446,447,448,449,522,526,609,518,519,520 $
    ,599,600,601,602,603,371,441,442,443,444,445,517,521,250,308,309,310,372,373,374,375 $
    ,311,312,313,314,315,316,376,377,205,253,254,255,256,257,258,317,155,156,157,158,201 $
    ,202,203,204,152,153,154,198,199,200,251,252,78,79,112,113,114,115,116,117,30,50     $
    ,51,52,53,80,81,82,1,2,11,12,13,27,28,29,531,532,533,534,614,615,616                 $
    ,617,383,384,385,386,454,455,456,457,536,537,538,619,620,621,622,623,387,458,459,460 $
    ,461,535,539,618,540,541,542,543,624,625,626,627,390,391,392,393,462,463,464,465,265 $
    ,266,267,268,325,326,327,328,319,320,321,322,323,324,388,389,206,259,260,261,262,263 $
    ,264,318,159,160,161,162,207,208,209,210,163,164,165,166,211,212,213,214,83,118,119  $
    ,120,121,122,123,124,56,57,58,84,85,86,87,88,3,14,15,31,32,33,54,55                  $
    ,554,555,556,557,639,640,641,642,402,403,404,405,475,476,477,478,550,551,552,633,634 $
    ,635,636,637,401,471,472,473,474,549,553,638,545,546,547,628,629,630,631,632,394,466 $
    ,467,468,469,470,544,548,269,329,330,331,395,396,397,398,332,333,334,335,336,337,399 $
    ,400,222,272,273,274,275,276,277,338,170,171,172,173,218,219,220,221,167,168,169,215 $
    ,216,217,270,271,89,90,125,126,127,128,129,130,37,59,60,61,62,91,92,93,4             $
    ,5,16,17,18,34,35,36,558,559,560,561,643,644,645,646,406,407,408,409,479,480         $
    ,481,482,563,564,565,648,649,650,651,652,410,483,484,485,486,562,566,647,567,568,569 $
    ,570,653,654,655,656,413,414,415,416,487,488,489,490,284,285,286,287,346,347,348,349 $
    ,340,341,342,343,344,345,411,412,223,278,279,280,281,282,283,339,174,175,176,177,224 $
    ,225,226,227,178,179,180,181,228,229,230,231,94,131,132,133,134,135,136,137,65,66    $
    ,67,95,96,97,98,99,6,19,20,38,39,40,63,64,581,582,583,584,668,669,670                $
    ,671,425,426,427,428,500,501,502,503,577,578,579,662,663,664,665,666,424,496,497,498 $
    ,499,576,580,667,572,573,574,657,658,659,660,661,417,491,492,493,494,495,571,575,288 $
    ,350,351,352,418,419,420,421,353,354,355,356,357,358,422,423,239,291,292,293,294,295 $
    ,296,359,185,186,187,188,235,236,237,238,182,183,184,232,233,234,289,290,100,101,138 $
    ,139,140,141,142,143,44,68,69,70,71,102,103,104,7,8,21,22,23,41,42,43 ]

    mir_act2act = fix(sort(act2mir_act))
    n_act_ring = [9,15,21,27,33,39,45,51,57,63,69,75,81]
    n_act_names = 3
    act_name = strarr(n_elements(act2mir_act), n_act_names)
    ring_format_str='(I2.2)'
    act_format_str='(I3.3)'
    act0 = 0
    for nr=0,adsec_shell_save.n_rings-1 do begin

        act_name[act0,0] = strtrim(indgen(adsec_shell_save.n_act_ring[nr])+act0, 2)
        act_name[act0,1] = strtrim(indgen(adsec_shell_save.n_act_ring[nr])+act0, 2)
        act_name[act0,2] = string(nr,FORMAT=ring_format_str) $
          + string(indgen(adsec_shell_save.n_act_ring[nr]),FORMAT=act_format_str)
        act0 = act0+adsec_shell_save.n_act_ring[nr]
    endfor


    act_coord = fltarr(2,n_elements(act2mir_act) )          ;; x,y coords of the acts
    act0 = 0
    for nr=0,adsec_shell_save.n_rings-1 do begin

        theta = findgen(adsec_shell_save.n_act_ring[nr])*(360.0/adsec_shell_save.n_act_ring[nr])+0.
        theta = !CONST.DtoR*transpose(theta)
        ;; act x,y coordinates
        act_coord[0,act0] = shift(adsec_shell_save.ring_radius[nr]*[cos(theta), sin(theta)],0,nr+1)
        act0 = act0+adsec_shell_save.n_act_ring[nr]
    endfor
    ;matching rigid M2 specification
    new_act_coord = [-act_coord[1,*], act_coord[0,*]]
    new_act_coord = new_act_coord[*,act2mir_act]
    
     

    if keyword_set(MAGELLAN) then begin
        ;true_act = mir_act2act[total(adsec_shell_save.n_act_ring[0:11])]
        true_act = mir_act2act[indgen(total(adsec_shell_save.n_act_ring[0:12]))]
        tmp = complement(true_act, indgen(672), dummy_act)
        adsec_save = {              $
            n_actuators: 672,    $
            true_act: true_act, $
            dummy_act: dummy_act, $
            act_coordinates: new_act_coord $
        }
    endif else begin
        true_act = indgen(672)
        dummy_act = -2
        adsec_save = {              $
            n_actuators: 672, $
            true_act: true_act , $
            dummy_act: dummy_act , $
            act_coordinates: new_act_coord $
        }
    endelse

    if adsec_save.dummy_act[0] ge 0 then begin
        act_name[act0:*,0] = strtrim(indgen(n_elements(adsec_save.dummy_act))+act0, 2)
        act_name[act0:*,1] = "XX"
        act_name[act0:*,2] = "XX"
    endif

    sc_save = {                 $
        act_name : act_name     $
    }
        

endelse

on_error, 2

val=reform(the_val)*1.0
if (size(val))(0) ne 1 then message,'Wrong input format'
if n_params() ne 1 then begin
    list = reform(the_list)
    nlist=n_elements(list)
	case n_elements(val) of
		adsec_save.n_actuators: begin
			val=val[list]
		end

		n_elements(list): begin
		end

		else: message, 'Wrong list size'
	endcase
endif else begin
	case n_elements(val) of
		adsec_save.n_actuators: begin
			val=val[adsec_save.true_act]
		end

		n_elements(adsec_save.true_act): begin
		end

		else: message, 'Wrong input size'
	endcase

	list=adsec_save.true_act
    nlist=n_elements(list)
    if n_elements(num_list) gt 0 then begin
        tmp = fltarr(adsec_save.n_actuators)
        tmp[num_list] = 1
        tmp = tmp[adsec_save.true_act]
        num_list = where(tmp)
    endif
endelse

ima=val

if n_elements(bar_title) eq 0 then bar_title=''
if n_elements(num_type) eq 0 then num_type=gr_save.num_type
if n_elements(noerase) eq 0 then noerase=0B
if n_elements(flag) eq 0 then flag=1
if n_elements(title) eq 0 then title=''
if n_elements(rot) eq 0 then rot = gr_save.theta_tv
if n_elements(refl) eq 0 then refl = gr_save.x_reflect_tv
;
; actuator geometry definition
;
if keyword_set(smooth) then begin
    x=adsec_save.act_coordinates(0,*)*(1.+randomn(seed,adsec_save.n_actuators)*1e-4)
    y=adsec_save.act_coordinates(1,*)*(1.+randomn(seed,adsec_save.n_actuators)*1e-4)
endif else begin
    x=adsec_save.act_coordinates(0,*)
    y=adsec_save.act_coordinates(1,*)
endelse

if n_elements(rot) ne 0 then begin
    t=rot*!CONST.DtoR
    matrot=transpose([[cos(t),sin(t)],[-sin(t),cos(t)]])
    dummy=matrot ## transpose([x,y])
    x=dummy[*,0]
    y=dummy[*,1]
endif

if keyword_set(refl) then x=-x

if n_elements(num_charthick) eq 0 then charthick=1.0
if n_elements(num_charsize) eq 0 then charsize=1.0
;the random term was added to correct a problem with the trigrid routine

;
; interpolation geometry definition
;
lim=fltarr(4)
gs1=fltarr(2)
if n_elements(map_samp) eq 0 then $
    if keyword_set(smooth) then map_samp=101 else map_samp=255;151
np=map_samp
lim(0)=-adsec_shell_save.out_radius
lim(1)=-adsec_shell_save.out_radius
lim(2)=adsec_shell_save.out_radius
lim(3)=adsec_shell_save.out_radius
gs1(0)=2*adsec_shell_save.out_radius/(np-1)
gs1(1)=2*adsec_shell_save.out_radius/(np-1)
x1=2*adsec_shell_save.out_radius*(findgen(np)/(np-1)-.5)*float(np-1)/np
y1=2*adsec_shell_save.out_radius*(findgen(np)/(np-1)-.5)*float(np-1)/np
;dis=shift(dist(np),(np-1)/2,(np-1)/2)/(np-1)*2*adsec_shell_save.out_radius
dis=shift(dist(np),(np-1)/2,(np-1)/2)/np*2*adsec_shell_save.out_radius
out=where((dis gt adsec_shell_save.out_radius) or (dis lt adsec_shell_save.in_radius))
in =where((dis le adsec_shell_save.out_radius) and (dis ge adsec_shell_save.in_radius))
;dummy=complement(out,lindgen(long(map_samp)^2),in,count)
if keyword_set(smooth) then begin
;   triangulate,x(list),y(list),tr,b
;   position=trigrid(x(list),y(list),ima,tr,gs1,lim,EXTRA=b)
    position=min_curve_surf(ima,x(list),y(list),GS=gs1,BOUNDS=lim,/TPS)
    if n_elements(min_pos) eq 0 then begin
    	min_pos=min(position[in])
    	no_lt=1B
    endif else no_lt=0B

    if n_elements(max_pos) eq 0 then begin
    	max_pos=max(position[in])
    	no_gt=1B
    endif else no_gt=0B

endif else begin
    ;spot_radius = 0.25*sqrt(!PI*(adsec_shell_save.out_radius^2-adsec_shell_save.in_radius^2)/adsec_save.n_actuators)
    ;position=fltarr(np,np)
    ;xcoord=rebin(x1,np,np,/s)
    ;ycoord=rotate(xcoord,1)
    ;for i=0,nlist-1 do begin
    ;    dummy=where(((xcoord-x(list(i)))^2+(ycoord-y(list(i)))^2) lt spot_radius^2)
    ;    position(dummy) =val(i)
    ;    if i eq 0 then in = dummy else in = [in, dummy]
    ;endfor
    if n_elements(spot_mag) eq 0 then spot_mag=0.5
    spot_radius = 0.5*spot_mag*sqrt(!PI*(adsec_shell_save.out_radius^2-adsec_shell_save.in_radius^2)/adsec_save.n_actuators)/(2*adsec_shell_save.out_radius)*np
    spot_diam = (round(2*spot_radius)/2*2+1) > 3
    spot_radius = spot_diam/2
    spot_map = fltarr(spot_diam,spot_diam)
    spot_map_idx = where(shift(dist(spot_diam),spot_radius,spot_radius) le spot_radius)
    spot_map(where(shift(dist(spot_diam),spot_radius,spot_radius) gt spot_radius))=0.0

    position=fltarr(np,np)
    if n_elements(no_smooth_val) ne 0 then begin
    	position[*,*]=no_smooth_val
    	spot_map(where(shift(dist(spot_diam),spot_radius,spot_radius) gt spot_radius))=no_smooth_val
    endif else begin
    	position[*,*]=min(val)
    	spot_map(where(shift(dist(spot_diam),spot_radius,spot_radius) gt spot_radius))=min(val)
    endelse
    xx=((round((x+adsec_shell_save.out_radius)/(2*adsec_shell_save.out_radius)*np - spot_radius)) > 0) < (np-spot_diam)
    yy=((round((y+adsec_shell_save.out_radius)/(2*adsec_shell_save.out_radius)*np - spot_radius)) > 0) < (np-spot_diam)
    for i=0,nlist-1 do begin
    	spot_map(spot_map_idx)=val[i]
        position[xx[list[i]],yy[list[i]]] = spot_map
        ;dummy=where(((xcoord-x(list(i)))^2+(ycoord-y(list(i)))^2) lt spot_radius^2)
        ;position(dummy) =val(i)
        ;if i eq 0 then in = dummy else in = [in, dummy]
    endfor
    if n_elements(min_pos) eq 0 then begin
    	min_pos=min(val)
    	no_lt=1B
    endif else no_lt=0B

    if n_elements(max_pos) eq 0 then begin
    	max_pos=max(val)
    	no_gt=1B
    endif else no_gt=0B
 ;   if n_elements(min_pos) eq 0 then min_pos=min(val)
 ;   if n_elements(max_pos) eq 0 then max_pos=max(val)
endelse

if n_elements(out_val) ne 0 then begin
    position(out)=out_val
endif else begin
    ave=mean(val)
    position(out)=ave
endelse



if keyword_set(NO_BACKGROUND) then position(out)=max_pos

if keyword_set(show) then goto,fine_dis

if keyword_set(COL_INVERT) then begin
	if total(replicate(!d.name,3) eq ['mac','x','win']) ge 1 then device,decompose=1
	background_old=!p.background
	color_old=!p.color
	!p.background=color_old
	!p.color=background_old
endif

if (flag eq 2) then begin
    shade_surf,min_v=min_pos,max_v=max_pos,position
endif

if (flag eq 1) then begin

    ;min_pos = min(val)
    ;max_pos = max(val)


    image_show, position, /as, sh=keyword_set(showbar), xax=x1, yax=y1 $
            , min_v=min_pos , max_v=max_pos, posiz=posiz, title=title $
            ,_EXTRA=plot_keywords, dig=2, BAR_TIT=bar_title, NOERASE=noerase $
            , NO_LT=no_lt, NO_GT=no_gt $
            ,YTICKF_BAR=ytickf_bar,XSTYLE=xstyle,YSTYLE=ystyle,YTITLE=ytitle $
            , LOG=log, INV=inv, XBAR=xbar, XTITLE=xtitle

    if not keyword_set(no_numbering) then begin
        top = !D.table_size
        if keyword_set(log) then begin
	        except=!except
	        ret=check_math(/print)
	        !except=0
	        if min(val) le 0 then message,'Values <=0',/cont
	        if keyword_set(inv) then begin
        		col=(bytscl(-alog10(val), MIN=min([-alog10(min_pos),-alog10(max_pos)]), MAX=max([-alog10(min_pos),-alog10(max_pos)]), TOP=top)+byte(top/2))<top
        	endif else begin
        		col=(bytscl(alog10(val), MIN=alog10(min_pos), MAX=alog10(max_pos), TOP=top)+byte(top/2))<top
        	endelse
			ret=check_math()
	        !except=except
        endif else begin
        	if keyword_set(inv) then begin
        		col=(bytscl(-val, MIN=min([-max_pos,-min_pos]), MAX=max([-min_pos,-max_pos]), TOP=top)+byte(top/2))<top
        	endif else begin

        		col=(bytscl(val, MIN=min_pos, MAX=max_pos, TOP=top)+byte(top/2))<top
        	endelse
        endelse

        if float(!version.release) lt 5.3 then begin
            can_use_colormap = 0
        endif else begin
            dev_name = !D.name
            if dev_name eq 'WIN' or dev_name eq 'X' or dev_name eq 'MAC' then begin
                can_use_colormap = colormap_applicable()
            endif else begin
                can_use_colormap = 1
            endelse
        endelse

        if not can_use_colormap then begin
            TVLCT, R, G, B, /GET
            col = r[col]+256L*g[col]+256L^2*b[col]
        endif

        xp=(x-lim(0))/(lim(2)-lim(0))*(posiz(2)-posiz(0))+posiz(0)
        yp=(y-lim(1))/(lim(3)-lim(1))*(posiz(3)-posiz(1))+posiz(1)

		if n_elements(ACT_PSYM) ne 0 then begin
	        for i=0,nlist-1 do begin
	            plots,xp(list[i]),yp(list[i]) $
	                , color=col(i), /device, PSYM=act_psym, SYMSIZE=act_symsize
	        endfor
	    endif else begin
	        if num_type ge 0 then begin
                    if n_elements(num_list) gt 0 then begin
                        for i=0,n_elements(num_list)-1 do begin
;                            if n_elements(list) eq n_elements(adsec_save.true_act) then begin
;                                tmp = fltarr()
;                                tmp[num_list] = 1
;                                num_list = where(tmp[list])
;                            endif
                            xyouts,xp(list[num_list[i]]),yp(list[num_list[i]]),sc_save.act_name[list[num_list[i]],num_type] $ ;strtrim(string(list[i]),2) $
                                   , color=col(num_list[i]),alignment=0.5,/device,CHARTHICK=num_charthick,CHARSIZE=num_charsize
                        endfor
                        for i=0,n_elements(second_list)-1 do begin
                            xyouts,xp(list[second_list[i]]),yp(list[second_list[i]]),sc_save.act_name[list[second_list[i]],num_type] $ 
                                   , color='00ff00'xl,alignment=0.5,/device,CHARTHICK=num_charthick,CHARSIZE=num_charsize
                        endfor
                    endif else begin
                        for i=0,nlist-1 do begin
                            xyouts,xp(list[i]),yp(list[i]),sc_save.act_name[list[i],num_type] $ ;strtrim(string(list[i]),2) $
                                   , color=col(i),alignment=0.5,/device,CHARTHICK=num_charthick,CHARSIZE=num_charsize
                        endfor
                    endelse

	        endif
	    endelse

    endif
endif

if keyword_set(COL_INVERT) then begin
	!p.background=background_old
	!p.color=color_old
endif

fine_dis:
end
