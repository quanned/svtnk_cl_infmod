                *ОГЛАВЛЕНИЕ СТАНДАРТНЫХ ПРОЦЕДУР*
* 1   BOX-пример применения
* 2   BOLBOX-пpимеp пpименения для ПРАВИЦЫ
* 3   VIB_DISK-процедура выбора диска с проверкй установки дискеты
* 4   METKA-процедура считывания метки с дискеты
* 5   GOTOV_LP-процедура проверки готовности печати с загрузкой LFONTA 0
* 6   SOOB-процедура сообщения об обработке информации
* 7   PEC-распечатка через системный вывод файлов типа .PRN
* 8   WIB_MEC-выбор наименования месяца по его номеру
* 9   PROPIS-возвращает сумму прописью
* 10  RAZDEL-процедура считывания имя текущего каталога
* 11  OKNO-процедура высвечивания окна по нарастающей
* 12  SIMVOL-функция пеpевода любого символа в большие pусские
* 13  FUN_A-пользовательская функция для ACHOICE
* 14  TAB_DBF-процедура преобразования сформ печати в файл .PRN в файл .DBF
* 15  HELP_US-Помощь при корректировках функцией DBEDIT()
* 16  NET_USE-функция проверки успешного открытия файла в сети
* 17  REC_LOCK функция проверит заблокирована ли запись и заблокирует для себя
* 18  ADD_REC функция создания пустой записи в сети с разделяеммым доступом
* 19  FIL_LOCK функция блокировки файла при работе в сети с разделенным дост
* 20  PEC2 -печать 2 экземрляра файла печати типа .PRN на этом же листе
* 21  FON-высвечивает обьемное окно
* 22  ITOG-выводит большие числа в удобо читаемой форме 9'123'345'678
* 23  VIB_MESK-выбор месяца через номер квартала через ACHOICE
* 24  VIB_MES-выбор месяца через ACHOICE
* 25  PACK_DBF-функция упаковки (надежная) файлов типа .DBF и DBT(с memo-полями)
* 26  PEC_LIST-печать .PRN файла с указанием номера листа
*1************************
ramka1='┌─┐│┘─└│▓'
ramka2='╔═╗║╝═╚║░'
clear
@ 1,1,23,79 BOX ramka1
@ 5,20,10,45 BOX ramka2
release all
inkey(0)

*2***********************
ramka1='┌─╧╙┘─└╙Ж'
ramka2='╔═╫╟╝═╚╟╨'
clear
@ 1,1,23,79 BOX ramka1
@ 5,10,10,25 BOX ramka2
release all
inkey(0)

*3*********************
PROC VIB_DISK
parameters disk      &&возвращает тип диска (пример DO DISKVIB with tip_d    ? tip_d   результат А)
ekr1=savescreen(8,37,14,43)
flag=0
ds=1
DO while .t.
   @ 8,37 to 14,43 double
   stor ' ' to disk
   set wrap on
   @9,38 say 'Диск:'
   @10,38 prom '  A  '
   @11,38 prom '  B  '
   @12,38 prom '  C  '
   @13,38 prom '  D  '
   menu to ds
   do case
      case ds=1
           disk='A'
           exit
      case ds=2
           disk='B'
           exit
      case ds=3
           disk='C'
           exit
      case ds=4
           disk='D'
           exit
      case ds=0
           flag=1
           exit
      other
           loop
   endcase
 enddo
imr='&disk'+':rab.rab'
ekr2=savescreen(10,28,14,50)
if flag=1
   RETURN
endif
do while .t.
   ahandle=FCREATE('&imr')
   if ferror() <> 0
      @ 10,28 to 14,50 double
      @ 11,29 say "     УСТАНОВИТЕ      "
      @ 12,29 say "      ДИСКЕТУ        "
      @ 13,29 say '          '+disk+'          '
      inkey(1)
      if lastkey()=27
         return
      endif
    else
      exit
    endif
enddo
 erase &imr
 restscreen(10,28,14,50,ekr2)
 restscreen(8,37,14,43,ekr1)
 release all
 return

*4********************************************
*METKA проверяет на корректность метки тома
PROC METKA
ft=.f.
parameters fdisk,met_tom,ft   &&  диск симв; метка тома симв; вернет истину или ложь
metds=fdisk+':'
!vol &metds > test.txt
per=fopen('test.txt')
buffer=freadstr(per,50)
per1=substr(buffer,25,11)
IF alltrim(per1)=alltrim(met_tom)
  ft=.t.
ELSE
  ft=.f.
       ekr=savescreen(11,21,15,46)
       @11,21 clear to 15,46
       @ 11,21 to 15,46
       @ 12,23 say '   Не та дискета!'
       @ 13,23 say ' Установи дискету '+met_tom
       @ 14,23 say '  Нажми любую клавишу'
       tone(500,3)
       inkey(0)
       restscreen(11,21,15,46,ekr)
ENDIF
fclose('test.txt')
erase test.txt
release all
RETURN

*5*********************
*Проверяет готовность печати с последующей загрузкой LFONTA 0
PROC GOTOV_LP
*ramka='╔═╗║╝═╚║'
ekr=savescreen(10,28,14,50)
DO while .not. isprinter()
       @ 10,28,14,50 BOX ramka
       @ 11,29 say "    ПОДГОТОВЬТЕ      "
       @ 12,29 say "     К РАБОТЕ        "
       @ 13,29 say "ПЕЧАТАЮЩЕЕ УСТРОЙСТВО"
ENDDO
restscreen(10,28,14,50,ekr)
release all
RETURN

*6***************************
**процедура сообщения об обработке информации
PROCEDURE SOOB
ramka='╔═╗║╝═╚║░'
@ 9,17,12,62 BOX ramka
@ 10,18 say '              В Н И М А Н И Е !             '
@ 11,18 say '  И Д Е Т  О Б Р А Б О Т К А  Д А Н Н Ы Х   '
RETURN

*7*********************************
*PECпечать  чеpез системный вывод
PROC PEC
PARAMETERS imj
imjf=imj+'.prn'
ekz=0
otp=0
tone(400,4)
@ 20,2 say ' Количество экземпляpов (1-6) ' get ekz pict '9' valid ekz > -1 .and. ekz < 7
read
DO GOTOV_LP
DO while .t.
  if ekz=0
     exit
  endif
  copy file &imjf to prn
*  !copy &imjf prn
   otp=otp+1
   ekz=ekz-1
   @22,2 say ' Отпечатано '+str(otp,1)+' экз '
ENDDO
erase &imjf
release all
RETURN

*8*********************************
* процедура выбора месяца
PROC WIB_MES
 parameters nms,mes
 DO CASE
  case nms=1
   mes='Январь  '
  case nms=2
   mes='Февраль '
  case nms=3
   mes='Март    '
  case nms=4
   mes='Апрель  '
  case nms=5
   mes='Май     '
  case nms=6
   mes='Июнь    '
  case nms=7
   mes='Июль    '
  case nms=8
   mes='Август  '
  case nms=9
   mes='Сентябрь'
  case nms=10
   mes='Октябрь '
  case nms=11
   mes='Ноябрь  '
  case nms=12
   mes='Декабрь '
 ENDcase
 release all
RETURN

*9**********************************
*PROPIS-процедура принимает максимальное число 9 999 999 999 и возвращает число прописью
PROC PROPIS
parameters numbr,prop     && numbr- ЧИСЛО ПЕРЕДАВАЕМОЕ ПРОЦЕДУРЕ 9 999 999 999
ns=str(numbr,10)          && prop- ВОЗВРАЩАЕТ СИМВОЛЬН ДИНАМИЧЕСКУЮ ПЕРЕМЕН
if numbr > 9999999999     && ПРОПИСЬЮ ЦЕЛОЕ ЧИСЛО
      tone(500,10)
      @prow()+1,1 say 'Не могу написать прописью!! (число больше 9 999 999 999 ...)'
      RETURN
endif
declare tab1[9]
tab1[1]='СТО '
tab1[2]='ДВЕСТИ '
tab1[3]='ТРИСТА '
tab1[4]='ЧЕТЫРЕСТА '
tab1[5]='ПЯТЬСОТ '
tab1[6]='ШЕСТЬСОТ '
tab1[7]='СЕМЬСОТ '
tab1[8]='ВОСЕМЬСОТ '
tab1[9]='ДЕВЯТЬСОТ '
**********************
declare tab2[9]
tab2[1]='ДЕСЯТЬ '
tab2[2]='ДВАДЦАТЬ '
tab2[3]='ТРИДЦАТЬ '
tab2[4]='СОРОК '
tab2[5]='ПЯТЬДЕСЯТ '
tab2[6]='ШЕСТЬДЕСЯТ '
tab2[7]='СЕМЬДЕСЯТ '
tab2[8]='ВОСЕМЬДЕСЯТ '
tab2[9]='ДЕВЯНОСТО '
***********************
declare tab3[11]
tab3[1]='ДЕСЯТЬ '
tab3[2]='ОДИНАДЦАТЬ '
tab3[3]='ДВЕНАДЦАТЬ '
tab3[4]='ТРИНАДЦАТЬ '
tab3[5]='ЧЕТЫРНАДЦАТЬ '
tab3[6]='ПЯТНАДЦАТЬ '
tab3[7]='ШЕСТНАДЦАТЬ '
tab3[8]='СЕМНАДЦАТЬ '
tab3[9]='ВОСЕМНАДЦАТЬ '
tab3[10] ='ДЕВЯТНАДЦАТЬ '
*************************
declare tab4[9]
tab4[1]='ОДНА '
tab4[2]='ДВЕ '
tab4[3]='ТРИ '
tab4[4]='ЧЕТЫРЕ '
tab4[5]='ПЯТЬ '
tab4[6]='ШЕСТЬ '
tab4[7]='СЕМЬ '
tab4[8]='ВОСЕМЬ '
tab4[9]='ДЕВЯТЬ '
***********************
declare tab5[9]
tab5[1]='ОДИН '
tab5[2]='ДВА '
tab5[3]='ТРИ '
tab5[4]='ЧЕТЫРЕ '
tab5[5]='ПЯТЬ '
tab5[6]='ШЕСТЬ '
tab5[7]='СЕМЬ '
tab5[8]='ВОСЕМЬ '
tab5[9]='ДЕВЯТЬ '
***********************
ed_mr=val(substr(ns,1,1))
st_m=val(substr(ns,2,1))
ds_m=val(substr(ns,3,1))
ed_m=val(substr(ns,4,1))
st_t=val(substr(ns,5,1))
ds_t=val(substr(ns,6,1))
ed_t=val(substr(ns,7,1))
st=val(substr(ns,8,1))
ds=val(substr(ns,9,1))
ed=val(substr(ns,10,1))
*****************************
if ed_mr > 0
   prop=prop+tab5[ed_mr]
   prop=prop+'МЛРД. '
endif
if st_m > 0
   prop=prop+tab1[st_m]
endif
   if ds_m=1
      ds_m1=ed_m+1
      prop=prop+tab3[ds_m1]
   endif
   if ds_m > 1
      prop=prop+tab2[ds_m]
   endif
  if ed_m > 0 .and. (ds_m > 1 .or. ds_m = 0)
     prop=prop+tab5[ed_m]
  endif
  if  st_m <> 0 .or. ds_m <> 0 .or. ed_m <> 0
     prop=prop+'МЛН. '
   endif
if st_t > 0
   prop=prop+tab1[st_t]
endif
   if ds_t=1
      ds_t1=ed_t+1
      prop=prop+tab3[ds_t1]
   endif
   if ds_t > 1
      prop=prop+tab2[ds_t]
   endif
  if ed_t > 0 .and. (ds_t > 1 .or. ds_t = 0)
     prop=prop+tab4[ed_t]
  endif
  if  st_t <> 0 .or. ds_t <> 0 .or. ed_t <> 0
     prop=prop+'ТЫС. '
   endif
***********************
  if st > 0
   prop=prop+tab1[st]
endif
   if ds=1
      ds1=ed + 1
      prop=prop+tab3[ds1]
   endif
   if ds > 1
      prop=prop+tab2[ds]
   endif
  if ed > 0 .and. (ds > 1 .or. ds = 0)
     prop=prop+tab5[ed]
  endif
  if ed_mr=0 .and. st_m=0 .and. ds_m=0 .and. ed_m=0 .and. st_t=0 .and. ds_t=0 .and. ed_t=0 .and. st=0 .and. ds=0 .and. ed=0
     prop=prop+'НОЛЬ '
  endif
release all
RETURN

*10****************************
*RAZDEL прочитет и вернет имя текущего ДИРЕКТОРИЯ
PROC RAZDEL
parameters im_dir   &&  вернет имя текущего директория
!dir > dir.txt
per=fopen('dir.txt')
buffer=freadstr(per,100)
im_dir1=substr(buffer,51,40)
pos=at(chr(13),im_dir1)
im_dir=substr(im_dir1,1,pos-1)
fclose('dir.txt')
erase dir.txt
release all
RETURN
*11*********************
*высветит полное окно по нарастающей с двойной рамкой
PROC OKNO
clear
ramka2='╔═╗║╝═╚║░'
*i=12
*j=12
*k=12
*l=67
*as=0
*FOR m=1 to 14
*    @ i,j,k,l BOX ramka2
     @0,0,24,79 BOX ramka2
*    inkey(0.01)
*    for t=1 to 15
*        as=as+1
*    next
*    i=i-1
*    j=j-1
*    k=k+1
*    l=l+1
*NEXT
release all
RETURN
********************************
* обращение пример replace art with SIMVOL(ART)
* переведет все символы  реквизита art в большие русские
FUNCTION  SIMVOL
PARAMETERS alfa1
ln=len(alfa1)
ln1=1
DO while ln1 <= ln
   alfa=substr(alfa1,ln1,1)
   DO CASE
      case alfa='q'.or.alfa='Q'.or.alfa='й'.or.alfa='Й'
           alfa='Й'
      case alfa='w'.or.alfa='W'.or.alfa='ц'.or.alfa='Ц'
           alfa='Ц'
      case alfa='e'.or.alfa='E'.or.alfa='у'.or.alfa='У'
           alfa='У'
      case alfa='r'.or.alfa='R'.or.alfa='к'.or.alfa='К'
           alfa='К'
      case alfa='t'.or.alfa='T'.or.alfa='е'.or.alfa='Е'
           alfa='Е'
      case alfa='y'.or.alfa='Y'.or.alfa='н'.or.alfa='Н'
           alfa='Н'
      case alfa='u'.or.alfa='U'.or.alfa='г'.or.alfa='Г'
           alfa='Г'
      case alfa='i'.or.alfa='I'.or.alfa='ш'.or.alfa='Ш'
           alfa='Ш'
      case alfa='o'.or.alfa='O'.or.alfa='щ'.or.alfa='Щ'
           alfa='Щ'
      case alfa='p'.or.alfa='P'.or.alfa='з'.or.alfa='З'
           alfa='З'
      case alfa='['.or.alfa='['.or.alfa='х'.or.alfa='Х'
           alfa='Х'
      case alfa='a'.or.alfa='A'.or.alfa='ф'.or.alfa='Ф'
           alfa='Ф'
      case alfa='s'.or.alfa='S'.or.alfa='ы'.or.alfa='Ы'
           alfa='Ы'
      case alfa='d'.or.alfa='D'.or.alfa='в'.or.alfa='В'
           alfa='В'
      case alfa='f'.or.alfa='F'.or.alfa='а'.or.alfa='А'
           alfa='А'
      case alfa='g'.or.alfa='G'.or.alfa='п'.or.alfa='П'
           alfa='П'
      case alfa='h'.or.alfa='H'.or.alfa='р'.or.alfa='Р'
           alfa='Р'
      case alfa='j'.or.alfa='J'.or.alfa='о'.or.alfa='О'
           alfa='О'
      case alfa='k'.or.alfa='K'.or.alfa='л'.or.alfa='Л'
           alfa='Л'
      case alfa='l'.or.alfa='L'.or.alfa='д'.or.alfa='Д'
           alfa='Д'
      case alfa=';'.or.alfa=':'.or.alfa='ж'.or.alfa='Ж'
           alfa='Ж'
      case alfa="'".or.alfa='"'.or.alfa='э'.or.alfa='Э'
           alfa='Э'
      case alfa='z'.or.alfa='Z'.or.alfa='я'.or.alfa='Я'
           alfa='Я'
      case alfa='x'.or.alfa='X'.or.alfa='ч'.or.alfa='Ч'
           alfa='Ч'
      case alfa='c'.or.alfa='C'.or.alfa='с'.or.alfa='С'
           alfa='С'
      case alfa='v'.or.alfa='V'.or.alfa='м'.or.alfa='М'
           alfa='М'
      case alfa='b'.or.alfa='B'.or.alfa='и'.or.alfa='И'
           alfa='И'
      case alfa='n'.or.alfa='N'.or.alfa='т'.or.alfa='Т'
           alfa='Т'
      case alfa='<'.or.alfa='б'.or.alfa='Б'
           alfa='Б'
      case alfa=','
           alfa=','
      case alfa='.'
           alfa='.'
      case alfa='>'.or.alfa='ю'.or.alfa='Ю'
           alfa='Ю'
      case alfa='/'.or.alfa='?'
           alfa='/'
      case alfa='ъ'.or.alfa='Ъ'.or.alfa='`'.or.alfa='~'
           alfa='Ъ'
      case alfa='m'.or.alfa='M'.or.alfa='ь'.or.alfa='Ь'
           alfa='Ь'
      otherwise
   ENDCASE
   if ln1=1
      I2=alfa
   else
      I2=I2+alfa
   endif
   ln1=ln1+1
ENDDO
return(I2)

*13----**************************************
function FUN_A        && пользовательская функция для динамического массива Ф И О
parameters mode,i,j  && паpаметpы функции ACHOICE  mode-режим i-текущий элемент массива j-относит поз в окне меню
DO CASE
   case mode = 3 .or. mode = 4    && режимы-исключение и нет варианта выбора
        if lastkey() = 13
           return(1)     && возвращает элемент курсора осуществляет выбор
        endif
        if lastkey()=27
            return(0)
        endif
        return(2)       && продолжение процесса выбора
   case mode = 1 .or.mode = 2 .or. mode = 0     && режимы ожидания курсор за пределами массива
        return(2)
ENDCASE

*14****************************************************************************
*TAB_DBF Процедура преобразов сформир печати в файл .PRN в файл типа .DBF
*Обращение DO TAB_DBF with 'form01',180
*где form01 имя файла типа .PRN 180-Размер записи файла который будет создан
*программно.Имя этого файла------TAB_DBF.DBF
PROC TAB_DBF
parameters imf,dlin
DO OKNO
@0,5 say ' Начало преобразования '+time()+' '
IF .not. file('opis_f.dbf')
   CREATE opis_f.dbf
   dan1='pole'
   dan2='C'
   dan3=dlin
   dan4=0
   use opis_f
   append blank
   repl field_name with dan1,field_type with dan2
   repl field_len with dan3,field_dec with dan4
   use
ENDIF
CREATE tab_dbf from opis_f
delete file opis_f.dbf
select 1
use tab_dbf
ZAP
select 2
im_prn=imf+'.prn'
IF .not. file('&im_prn')
    clear
    tone(200,3)
    @12,22 say 'В вашем каталоге нет файла '+ im_prn
    inkey(3)
    RETURN
ENDIF
log=fopen('&im_prn')
count1=0
col=1
odins=' '
count=0
ft=.t.
kol_bit=fseek(log,0,2)
fseek(log,0)
DO WHILE ft=.t.
   stroka=' '
   DO WHILE odins <> chr(10)
      fseek(log,0,1)
      odins=freadstr(log,1)
      IF odins='Н'
*         @12,38 say str(asc(odins),3)
         odins='H'
      ENDIF
      count=count+1
      count1=count1+1
      if count1=1000
         count1=0
         col=col+1
         tone(200,1)
         @1,col say chr(1)
      endif
      IF count=kol_bit
         ft=.f.
         exit
      ENDIF
      stroka=stroka+odins
    ENDDO
   bufer=memotran(stroka,' ',' ')
   select 1
   append blank
   replace pole with bufer
   odins=' '
   select 2
ENDDO
tone(200,5)
@24,45 say ' Конец пребразовния '+time()+' '
inkey(0)
clear
release all
close all
RETURN

*15-------************************************************
PROC HELP_US
box0='        '
box1='┌─╖│╜─└│'
box2=box1
@3,30,23,75 box box1
@ 22,30 clear to 22,75
e=31
@ 4,30 say ' '
ns=row()
@ row(),e say  '                                            '
@ row()+1,e say  '                                            '
@ row()+1,e say  '                 - курсор вниз              '
@ row()+1,e say  '                 - курсор вверх             '
@ row()+1,e say  '                 - страница вниз            '
@ row()+1,e say  '                 - страница вверх           '
@ row()+1,e say '                 - в начало записи          '
@ row()+1,e say '                 - в конец записи           '
@ row()+1,e say '                 - пополнение               '
@ row()+1,e say '                 - удаление/восстан.        '
@ row()+1,e say '                 - поиск                    '
@ row()+1,e say '                 - выход                    '
@ row()+1,e say '                 - в начало файла           '
@ row()+1,e say '                 - в конец файла            '
@ row()+1,e say '                                            '
@ row()+1,e say '                                            '
@ 6,42 say ' '
@ row(),42 say chr(25)+' '
@ row()+1,42 say chr(24)+' '
e=38
@ row()+1,e say  ' PgDn '
@ row()+1,e say  ' PgUp '
@ row()+1,e say ' Home '
@ row()+1,e say ' End  '
@ row()+1,e say ' INS  '
@ row()+1,e say ' DEL  '
@ row()+1,e say ' F2   '
@ row()+1,e say ' ESC  '
@ row()+1,e say ' Ctrl/PgUp'
@ row()+1,e say ' Ctrl/PgDn'
@ row()+1,e say '═════════════════════════════'
@ row()+1,e say '  Нажмите любую клавишу...   '
@ row()+1,31 clear to 22,74
  @ ns+1,35,row(),70 box box2
a=0
do while a=0
a=inkey()
enddo
RETURN

************************************
*16----- *********************  Функция проверки открытия файла в сети ******
*---Обращение к функции
* DO WHILE .not. NET_US('j:\comun\m031601','sprav',.f.)
* ENDDO

FUNCTION net_use
PARAMETERS file,psevdo, ex_use
PRIVATE inch
inch=0
okno=savescreen(0,0,2,70)
DO WHILE .T.

   IF ex_use                    && исключительное пользование
      USE &file alias &psevdo EXCLUSIVE
   ELSE
      USE &file alias &psevdo           && обычное
   ENDIF

   IF .NOT. neterr()           && успешное открытие
      restscreen(0,0,2,70,okno)
      RETURN (.T.)
   ENDIF
   tone(300,3)
   @ 0,1 SAY "  Занят файл "+FILE+".Выход-ESC  "
   inch=INKEY(0.5)
   IF inch=27
      restscreen(0,0,2,70,okno)
      RETURN (.F.)
   ENDIF
ENDDO
restscreen(0,0,2,70,okno)
RETURN (.T.)
* End - NET_USE

*17**************функция проверит заблокирована ли запись и заблокирует для себя
*ОБРАЩЕНИЕ
*DO WHILE .not. rec_lock()
*    loop
*ENDDO
*delete
*unlock
FUNCTION rec_lock
PRIVATE inch
inch=0

IF RLOCK()
   RETURN (.T.)         && успешно заблокировали
ENDIF

DO WHILE .T.

   IF RLOCK()
   @ 0,1 CLEAR TO 0,79
      RETURN (.T.)              && locked
   ENDIF

   @ 0,1 SAY "  Занята запись"+".Выход-ESC  "
   inch=INKEY(0.5)
   IF inch=27
      @ 0,1 CLEAR TO 0,79
      RETURN (.F.)
   ENDIF
ENDDO
RETURN (.F.)                    && nнеуспешное блокирование
* End - REC_LOCK

***18 ******** функция создания пустой записи в сети с разделяеммым доступом
*ОБРАЩЕНИЕ
*DO WHILE .not. ADD_REC()
*   loop
*ENDDO
***************************
FUNCTION add_rec
APPEND BLANK
IF .NOT. neterr()
   RETURN (.T.)
ENDIF
PRIVATE inch
inch=0
@ 0,1 SAY "  Занята запись"+".Выход-ESC  "

DO WHILE .T.

   APPEND BLANK
   IF .NOT. neterr()
      @ 0,1 CLEAR TO 0,79
      RETURN (.T.)
   ENDIF
   @ 0,1 SAY "  Занята запись"+".Выход-ESC  "
   inch=INKEY(0.5)
   IF inch=27
      @ 0,1 CLEAR TO 0,79
      RETURN (.F.)
   ENDIF
ENDDO
RETURN (.F.)                    && not locked
* End ADD_REC

* 19******* функция блокировки файла*********************
*ОБРАЩЕНИЕ
*DO WHILE .not. fil_lock()
*   loop
*ENDDO
*---------------------------------------
FUNCTION fil_lock
PRIVATE inch
inch=0

IF FLOCK()
   RETURN (.T.)         && locked
ENDIF

DO WHILE .T.

   IF FLOCK()
      @ 0,1 CLEAR TO 0,79
      RETURN (.T.)              && locked
   ENDIF
   tone(300,3)
   @ 0,1 SAY "  Занят файл "+".Выход-ESC  "
   inch=INKEY(0.5)
   IF inch=27
      @ 0,1 CLEAR TO 0,79
      RETURN (.F.)
   ENDIF

ENDDO
RETURN (.F.)                    && not locked
* End - FIL_LOCK
*-------------------------------------------------
*     Процедура печати табуляграмм в 2х экземплярах на одном листе
*     Обращение DO PEC2 with 'forma001',100
*     где forma001 имя полученного файла с расширением .prn
*     100 максимальная длина строки в файле .prn
*     для вывода полученного файла NEW.PRN
*     DO PEC with 'new'
PROC PEC2
parameters imf,dlina
imtf=imf+'.prn'
log=fopen('&imtf')
outf=fcreate("new.prn")
leng=fseek(log,0,2)        &&определяем длину файла
leng1=leng
fseek(log,0)
buffer=freadstr(log,leng)
cou=0
n=1
cou=at(chr(10),buffer)
fwrite(outf,buffer,cou)
if ferror() <> 0
   ? "не могу записать " +str(ferror(),3)
   fclose(log)
   fclose(outf)
   inkey(0)
   return
endif
DO WHILE .t.
   fseek(log,cou+1,0)
   leng1=leng-cou
   buffer=freadstr(log,leng1)
   if len(buffer) = 0
      exit
   endif
   cou1=at(chr(10),buffer)
   if cou1 = 0
      exit
   endif
   bf=substr(buffer,1,cou1-2)
   *------- замена уpпавл символов
   zam=chr(13)+repl(' ',dlina+4)
   DO while .t.
      vx=at(chr(13),bf)
      if vx=0
         exit
      endif
      bfm=stuff(bf,vx,vx,'')
      bf=bfm
   ENDDO
   prob=repl(' ',dlina - len(bf) + 4)
   buffer1=bf+prob+bf+chr(13)+chr(10)
   fwrite(outf,buffer1)       &&,cou1)
   cou=cou+cou1
   if ferror() <> 0
      ? "не могу записать " +str(ferror(),3)
      fclose(outf)
      fclose(log)
      inkey(0)
      return
   endif
ENDDO
fclose(outf)
fclose(log)
release all
RETURN
* 21 ************************************
*       Обращение DO FON with 5,10,10,30
*           где 5,10,10,30  координаты окна
PROC FON
parameters a,b,c,d
ramka= '▓▓▓▓▓▓▓▓▓'
ramka2='┌─┐│┘─└│ '  &&░'
@a,b,c,d BOX ramka
@a+1,b+3,c+1,d+3 BOX ramka2
RETURN

*22 *****************************************************************************
                                 ******  Процедура выделения разрядов  ******
*   обращение DO ITOG with cislo,slovo
*   где cislo макс число=999 999 999 999 возвр симв перем999'999'999'999

PROCEDURE ITOG
parameters a,slovo
b=int(a)
dr=a-b
for i = 1 to 100
  j=str(i,len(alltrim(str(i))))
  l=len(alltrim(str(b)))
  b1=str(b,l)
  if l<=3
    tr&j=b1
    slovo=tr&j+slovo
    exit
  else
    tr&j=substr(b1,l-2,3)
    b=val(substr(b1,1,l-3))
    slovo="'"+tr&j+slovo
  endif
next
slovo=slovo+substr(alltrim(str(dr)),2)
return

** 23  ***Выбор наименования и ном мес через запрос квартала
*                   naimes=space(8)
*                   nomer=0
*** обращение       DO VIB_MESK  with naimes,nomer
*   вернет(пример) 'февраль ',2

PROC VIB_MESK
parameters maimes,nomer
set colo to
IF iscolor()
   set color to w+/b
ELSE
   set color to w+/n
ENDIF
save screen to ekr
declare maskvr[4]
maskvr[1]='Квартал 1 '
maskvr[2]='Квартал 2 '
maskvr[3]='Квартал 3 '
maskvr[4]='Квартал 4 '
@3,3 say chr(25)
@7,16 say chr(24)
@2,4 to 7,15
@3,5 clear to 6,14
ft=.t.
d_kv=ACHOICE(3,5,7,14,maskvr,ft,'fun_a',1)
if lastkey()=27
   restore screen from ekr
   release all
   RETURN
endif
declare masmes[3]
@2,13 say chr(25)
@5,26 say chr(24)
@1,14 to 5,25
@2,15 clear to 4,24

DO CASE
   case d_kv = 1
        masmes[1]=' Январь       '
        masmes[2]=' Февраль      '
        masmes[3]=' Март         '
        ft=.t.
        d_mes=ACHOICE(2,15,4,24,masmes,ft,'fun_a',1)
        DO CASE
           case d_mes=1
                naimes='Январь  '
                nomer=1
           case d_mes=2
                naimes='Февраль '
                nomer=2
           case d_mes=3
                naimes='Март    '
                nomer=3
          ENDCASE
   case d_kv=2
        masmes[1]=' Апрель       '
        masmes[2]=' Май          '
        masmes[3]=' Июнь         '
        d_mes=ACHOICE(2,15,4,24,masmes,ft,'fun_a',1)
        DO CASE
           case d_mes=1
                naimes='Апрель  '
                nomer=4
           case d_mes=2
                naimes='Май     '
                nomer=5
           case d_mes=3
                naimes='Июнь    '
                nomer=6
         ENDCASE
   case d_kv=3
        masmes[1]=' Июль         '
        masmes[2]=' Август       '
        masmes[3]=' Сентябрь     '
        d_mes=ACHOICE(2,15,4,24,masmes,ft,'fun_a',1)
        DO CASE
           case d_mes=1
                naimes='Июль    '
                nomer=7
           case d_mes=2
                naimes='Август  '
                nomer=8
           case d_mes=3
                naimes='Сентябрь'
                nomer=9
         ENDCASE
   case d_kv=4
        masmes[1]=' Октябрь      '
        masmes[2]=' Ноябрь       '
        masmes[3]=' Декабрь      '
        d_mes=ACHOICE(2,15,4,24,masmes,ft,'fun_a',1)
        DO CASE
           case d_mes=1
                naimes='Октябрь '
                nomer=10
           case d_mes=2
                naimes='Ноябрь  '
                nomer=11
           case d_mes=3
                naimes='Декабрь '
                nomer=12
         ENDCASE
ENDCASE
restore screen from ekr
release all
RETURN

** 24  ***Выбор наименования и ном мес
*                   naimes=space(8)
*                   nomer=0
*** обращение       DO VIB_MES  with naimes,nomer
*   вернет(пример) 'февраль ',2

PROC VIB_MES
parameters naimes,nomer
set colo to
IF iscolor()
   set color to w+/b
ELSE
   set color to w+/n
ENDIF
save screen to ekr
declare masmes[12]
masmes[1]=' Январь  '
masmes[2]=' Февраль '
masmes[3]=' Март    '
masmes[4]=' Апрель  '
masmes[5]=' Май     '
masmes[6]=' Июнь    '
masmes[7]=' Июль    '
masmes[8]=' Август  '
masmes[9]=' Сентябрь'
masmes[10]=' Октябрь '
masmes[11]=' Ноябрь  '
masmes[12]=' Декабрь '
@7,2 say chr(25)
@18,15 say chr(24)
@6,3 to 19,14
@7,4 clear to 18,13
ft=.t.
d_ms=ACHOICE(7,4,18,13,masmes,ft,'fun_a',1)
if lastkey()=27
   restore screen from ekr
   release all
   RETURN
endif
DO CASE
   case d_ms = 1
        naimes=masmes[1]
        nomer=1
   case d_ms = 2
        naimes=masmes[21]
        nomer=2
   case d_ms = 3
        naimes=masmes[3]
        nomer=3
   case d_ms = 4
        naimes=masmes[4]
        nomer=4
   case d_ms = 5
        naimes=masmes[5]
        nomer=5
   case d_ms = 6
        naimes=masmes[6]
        nomer=6
   case d_ms = 7
        naimes=masmes[7]
        nomer=7
   case d_ms = 8
        naimes=masmes[8]
        nomer=8
   case d_ms = 9
        naimes=masmes[9]
        nomer=9
   case d_ms = 10
        naimes=masmes[10]
        nomer=10
   case d_ms = 11
        naimes=masmes[11]
        nomer=11
   case d_ms = 12
        naimes=masmes[12]
        nomer=12
   case d_ms = 3
ENDCASE
restore screen from ekr
release all
RETURN

*** 25 функция PACK_DBF() упакует ваш файл(безопасно) и что важно,
*      если имеется файл .DBT (с memo-полями) он тоже будет упакован,
*      что не делает команда PACK
*      Обращение PACK_DBF(имя файла DBF без расширения)
*      PACK_DBF('mttn001')

FUNCTION PACK_DBF
parameters im_dbf

im_dbfp=alltrim(im_dbf)+'.dbf'
im_dbtp=alltrim(im_dbf)+'.dbt'

RENAME &im_dbfp to vrem.dbf

IF FILE(im_dbtp)
   RENAME &im_dbtp to vrem.dbt
ENDIF
soxr_sel=select()
SELECT 0
USE vrem.dbf
COPY TO &im_dbf FOR .not. DELETED()
USE

DELETE FILE vrem.dbf

IF FILE('vrem.dbt')
   DELETE FILE vrem.dbt
ENDIF

SELECT(soxr_sel)

RETURN('')

********26 PEC_LIST ************

PROC PEC_LIST
parameters imf
clear
*set proc to proc_f
*pec_list Процедура печати файла(с pасшиpением PRN) с указанного листа
*поиск по совпадению 'лист '+str(nlst,3)
imtf=imf+'.prn'
if .not. file('&imtf')
   tone(600,3)
   @ 13,15 say 'Нет файла печати '+imtf
   inkey(0)
   return
endif
nlst=1
kolpc=1
@ 21,18 say 'Укажи количество экземпляpов ' get kolpc pict '9'
read
if kolpc=0
   return
endif
log=fopen('&imtf')
outf=fcreate("new_p.prn")
leng=fseek(log,0,2)    &&определяем длину файла
leng1=leng
fseek(log,0)
buffer=space(512)
block=512
bytes=fread(log,@buffer,block)
if bytes <> 0
   cu=at(chr(10),buffer)
   if cu <> 0
      fwrite(outf,buffer,cu)
   endif
else
   fclose(outf)
   fclose(log)
   return
endif
do while .t.
   @ 23,0 clea to 23,79
   fseek(log,cu+1,0)
   @ 22,18 say 'Укажи N листа, с которого печатать ' get nlst pict '999'
   read
   if lastkey()  = 27
       fclose(outf)
       fclose(log)
       return
   endif
   if nlst = 1
      for rkol=1 to kolpc
          DO GOTOV_LP
          copy  file &imtf to prn  && > nul
      next
      fclose(outf)
      fclose(log)
      erase new_p.prn
      return
   endif
   perl='лист '+str(nlst,3)
   bl=0
   do while .t.
      bytes=fread(log,@buffer,block)
      if bytes <> 0
         cou=at(perl,buffer)
         if cou <> 0
            prob=repl(' ',5)
            buffer1=prob+perl+chr(13)+chr(10)
            fwrite(outf,buffer1)
            cou1=at(chr(10),substr(buffer,cou))
            exit
         else
            bl=bl+bytes
            fseek(log,bl+1,0)
         endif
       else
         tone(600,3)
         @ 23,20 say 'Нет листа '+str(nlst,3)+ ' в файле печати '+imtf
         inkey(0)
         exit
       endif
   enddo
   if bytes = 0
      loop
   else
       exit
   endif
enddo
tone(500,4)
pozp=cou+cou1+1+bl
    buffer=space(512)
    DO WHILE .t.
      fseek(log,pozp,0)    && УСТАНОВКА ТЕКУЩЕЙ ПОЗИЦИИ
      leng1=leng-pozp
      if leng1 > 65000
         leng1=65000
      endif
      pozp=pozp+leng1
      buffer=freadstr(log,leng1)
      if len(buffer) = 0
         exit
      endif
      fwrite(outf,buffer)       &&,cou1)
      if ferror() <> 0
         ? "не могу записать " +str(ferror(),3)
         inkey(0)
         fclose(outf)
         fclose(log)
         return
       endif
   ENDDO
fclose(outf)
fclose(log)
for rkol=1 to kolpc
    DO GOTOV_LP
    copy  file new_p.prn to prn  && > nul
next
erase new_p.prn
release all
return
*************
