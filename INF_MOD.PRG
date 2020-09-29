set century ON
set exclusive OFF
set deleted ON
set procedure to ST_PROC
set confirm on
set safe off
set talk off
set color to
if iscolor()
   set color to w+/b
else
set color to w+/n,n/w
endif
set date german
set scor off
set wrap on
*inf_mod.prg -прогр. контроля тех. процесса готовности нов. модели для просчета цен

   * На каждой ПЭВМ где установлена эта программа в каталоге С:\NADO\infzap.dbf
   * должна быть следуящая информация: поле rek1 файла infzap.dbf
   *                               rek1=0 - в бюро планирования цен
   *                               rek1=1 - в тех бюро
   *                               rek1=2 - в ОТИЗ
   *                               rek1=3 - в ОНИС
   *                               rek1=4 - в НСИ и стандартизаторы
ramka1='┌─┐│┘─└│ '
ramka3='░░░░░░░░░'
set cursor on
set century ON
clea
store 0 to nn,mn
select 1
use C:\NADO\infzap EXCLUSIVE
prz1=rek1
use
pp=0
DO while .t.
   ramka='╔═╗║╝═╚║░'
   clear
   @1,0,23,79 BOX ramka
   @23,64 say ' ESC-Выход '
   @7,12 say '  И Н Ф О Р М А Ц И Я  П О  Н О В Ы М  М О Д Е Л Я М  '
   @0,4,2,17 BOX ramka1
   @0,23,2,36 BOX ramka1
   @0,42,2,55 BOX ramka1
   @0,61,2,74 BOX ramka1
   @22,4,24,27 BOX ramka1
   @22,32,24,55 BOX ramka1
   @22,58,24,75 BOX ramka1
   @10,6,12,33 BOX ramka1
   @10,46,12,73 BOX ramka1
   @14,26,16,55 BOX ramka1
   @18,3,20,39 BOX ramka1
   @18,43,20,76 BOX ramka1
   set wrap on
   mn=nn
   @1,5  prom ' КОРРЕКТ.  '
   @1,24 prom 'ПЕЧАТЬ ПЭО '
   @1,43 prom 'ИНДЕКСАЦИЯ '
   @1,62 prom ' УПАКОВКА  '
   @23,5 prom 'ПЕЧ ТЕХН(ц не обсчит)'
   @23,33 prom 'ПЕЧ ТЕХН(ц обсчитаны)'
   @23,59 prom 'ПРОСМОТР(ц обсч)'
   @11,7 prom 'ПРОСМОТР моделей для оОТиЗ'
   @11,47 prom ' ПЕЧАТЬ моделей за период '
   @15,27 prom ' ПЕЧ ТЕХН(ц не обсчит)план '
   @19,4 prom ' Проверка готовности модели(infmod)'
   @19,44 prom ' Проверка обсчит.моделей(cenmod)'
   menu to mn
   DO CASE
      case mn=1
           nn=mn
           DO KOR_INF1
      case mn=2
           nn=mn
           if prz1=0
              pp=0
              DO PEC_INF1
           else
              @1,24 say '  НЕ ДЛЯ ВАС!  '
              tone(500,5)
              inkey(1)
           endif
      case mn=3
           nn=mn
           DO IND_INF1
      case mn=4
           nn=mn
           DO UPK_INF1
      case mn=5
           nn=mn
           if prz1=1 .or. prz1=0
              pp=1
              DO MEN_PEC1
           else
              @23,5 say '  НЕ ДЛЯ ВАС!  '
              tone(500,5)
              inkey(1)
           endif
      case mn=6
           nn=mn
           if prz1=1 .or. prz1=0
              pp=1
              DO PEC_INF2
           else
              @23,5 say '  НЕ ДЛЯ ВАС!  '
              tone(500,5)
              inkey(1)
           endif
      case mn=7
           nn=mn
           if prz1=1 .or. prz1=0
              pp=1
              DO PRS_INF2
           else
              @23,59 say 'НЕ ДЛЯ ВАС!'
              tone(500,5)
              inkey(1)
           endif
      case mn=8
           nn=mn
           if prz1=2
              pp=0
              DO COR_OTIZ
           else
              @1,24 say '  НЕ ДЛЯ ВАС!  '
              tone(500,5)
              inkey(1)
           endif
      case mn=9
           nn=mn
           if prz1=3
              pp=1
              DO PEC_DAT
           else
              @13,31 say '  НЕ ДЛЯ ВАС!   '
              tone(500,5)
              inkey(1)
           endif
      case mn=10
           nn=mn
           if prz1=1 .or. prz1=0 .or. prz1=3
              pp=1
              DO PEC_INF5
           else
              @5,30 say '     НЕ ДЛЯ ВАС!     '
              tone(500,5)
              inkey(1)
           endif
      case mn=11
           nn=mn
           if prz1=3
              pp=1
              DO ERROR
           else
              @13,31 say '  НЕ ДЛЯ ВАС!   '
              tone(500,5)
              inkey(1)
           endif
      case mn=12
           nn=mn
           if prz1=3
              pp=1
              DO ERROR2
           else
              @13,31 say '  НЕ ДЛЯ ВАС!   '
              tone(500,5)
              inkey(1)
           endif
      case mn= 0
           close all
           release all
           exit
   ENDCASE
ENDDO
clear
RETURN
********************************************************************
PROC PEC_DAT        && печать за период
DO OKNO
@0,5 say '[ Формирование печати за заданный период ]'
@3,12 say '[ Введите период за который хотите получить информацию! ]'
n_dat=ctod(space(8))
k_dat=ctod(space(8))
@5,12 say '[ Начальная дата(чч.мм.гггг)-> ]' get n_dat
@7,12 say '[ Конечная дата(чч.мм.гггг)->  ]' get k_dat
read
if lastkey()=27
   close all
   release all
   RETURN
endif
DO OKNO
@0,5 say ' Формирование печати  '
DO SOOB
select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod
COPY TO c:\infmod FOR dat>=n_dat .and. dat<=k_dat
use
use c:\infmod alias infgns exclusive
index on dtos(datg)+mod to c:\infmod
set index to c:\infmod

lst=1
ks=68
pc=2
pcd=15
set console off
set device to print
set printer to c:\f_imodd
@prow(),pcol() say chr(27)+chr(33)+chr(1)    && расширенный шрифт
DO SH_MODD
DO WHILE .not. EOF()
   detstr='      '+dtoc(dat)+'       '+mod
   @prow()+1,1 say detstr
   if prow() = ks
      DO S_MODD
   endif
   SKIP
ENDDO
@prow()+3,5 say 'Конец инфоpмации  '+dtoc(date())
@prow(),1 say ' '
@prow(),pcol() say chr(27)+chr(33)+chr(5)      && установка CONDENSED
set printer to
set device to screen
set console on
clear                      &&   чистим экран
DO SH_MODD1                 &&   выводим на экран шапку
s = 6                      &&   с 6 строки выводим инф-цию (5 строк на шапку)
go top
DO WHILE .not. EOF()
   @ s,1 say '      '+dtoc(dat)+'       '+mod       &&  выводим первую строку
   s=s+1                                             &&  формируем следующую
   if s>23                                           &&  если кол-во строк > 23
     inkey(0)                                        &&  enter
     s=6                                             &&  переходим опять на 6 строку
     @ 6,1 clear to 24,78                            &&  чистим экран с 6 по 24
   endif
   SKIP
ENDDO
inkey(0)
clear
DO PEC with 'c:\f_imodd.prn'
release all
close all
erase c:\f_imodd.txt
RETURN
*************** проц. головная шапка (экран)*****************************
PROC SH_MODD1
@ 1,10 say ' П е р е ч е н ь   моделей '
@ 2,5 say 'за период с '+dtoc(n_dat)+' по '+dtoc(k_dat)
@ 3,5 say repl('-',36)
@ 4,5 say '     Дата    :      Модель       '
@ 5,5 say repl('-',36)
RETURN
************** проц. головная шапка ********************************
PROC SH_MODD
cherta=repl('-',34)
@prow()+3,10 say ' П е р е ч е н ь   моделей '
@prow()+1,5 say 'за период с '+dtoc(n_dat)+' по '+dtoc(k_dat)
if lst > 1
   @prow()+1,20 say 'стр.'+str(lst,2)
endif
@prow()+1,5 say cherta
@prow()+1,5 say ':    Дата    :      Модель       :'
@prow()+1,5 say cherta
@prow()+1,5 say ':     1      :         2         :'
@prow()+1,5 say cherta
RETURN
**************** проц. сокр. шапка **********************************
PROC S_MODD
lst=lst+1
setprc(0,0)
cherta=repl('-',34)
@prow()+2,3 say '---- линия разреза --------'
@prow()+1,20 say 'стр.'+str(lst,2)
@prow()+1,5 say cherta
@prow()+1,5 say ':     1      :         2         :'
@prow()+1,5 say cherta
RETURN
********************************************************************
PROC KOR_INF1           &&-корректировка
set exclusive OFF
select 0
IF .not. net_use('j:\texno\sprtx','fio',.f.)
   close all
   release all
   RETURN
endif
set index to j:\texno\sprtx

select 0
IF .not. net_use('j:\nsig\cenmod','cenmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod,j:\nsig\cenmod1

select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod

select 0
if .not. NET_USE('j:\texno\m101001','m101001',.f.)
  release all
  close all
  return
endif
set index to j:\texno\m101001

select 0
if .not. NET_USE('j:\nsi_nov\sprrvm','sprrvm',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprrvm4,j:\nsi_nov\sprrvm,j:\nsi_nov\sprrvm1,j:\nsi_nov\sprrvm2,j:\nsi_nov\sprrvm3

select 0
if .not. NET_USE('j:\nsi_nov\nesvm','nesvm',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\nesvm,j:\nsi_nov\nesvm1

select 0
IF .not. net_use('j:\nsi_nov\sprnop','polotno',.f.)
   close all
   release all
   return
endif
set index to j:\nsi_nov\sprnop,j:\nsi_nov\sprnop1,j:\nsi_nov\sprnop2

select 0
if .not. NET_USE('j:\nsi_nov\sprvc','sprvc',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprvc

select 0
IF .not. net_use('j:\nsig\infgns','infgns',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infgns,j:\nsig\infgns1

select infmod

DECLARE pole[12]
pole[1]='prz'
pole[2]='dat'
pole[3]='mod'
pole[4]='tex'
pole[5]='tex_p'
pole[6]='mes'
pole[7]='fio'
pole[8]='otiz_r'
pole[9]='otiz'
pole[10]='gns'
pole[11]='stan'
pole[12]='datg'

DECLARE shapka[12]
shapka[1]='П'
shapka[2]='Дата'
shapka[3]='Модель'
shapka[4]=' техн;информ'
shapka[5]='техн; пош'
shapka[6]='Мес;плн'
shapka[7]=' Ф.И.О. ; технолога'
shapka[8]='ПЗЦ;раск'
shapka[9]='ОТИЗ;тр/з'
shapka[10]='ГНС'
shapka[11]='Ст'
shapka[12]='Дата; ГНС'
flag_u=0
r_fio=''
DO OKNO
@0,4 say '[ Если вы ввели в ЭВМ всю информацию по модели занесите в свою графу 1 ]'
@24,5 say '[ F1-Помощь ]'
if prz1=3
   @24,40 say '[ F10-Проверка готовности модели ]'
endif
if prz1=1
   @24,25 say '[ Для ввода новой модели нажмите клавишу <Insert> ]'
   @24,2 say '[ F5 - печать ]'   &&& 12.05.2011
endif
DBEDIT (4,2,20,77,pole,'f_INF1','',shapka)
close all
release all
RETURN
******************** функция пользователя f_INF1 ********************
FUNCTION f_INF1
PARAMETERS mode,i
set cursor on
private cur_pole
cur_pole=pole[i]
DO CASE
   CASE mode < 4
        if i=1
           keyboard chr(4)         && куpсоp на 2 поле
        endif
        @2,10 say ' Дата: '+dtoc(dat)
        @2,40 say ' Модель: '+mod
        r_mod=mod
        select m101001
        seek r_mod
        if found()=.t.
           @22,36 say ' '+prim+' '
        else
           @22,36 say ' '+space(10)+' '
        endif
        select infmod
        return(1)
   CASE lastkey()=27                    && ESC-выход из корректировки
        close all
        clear
        return(0)
   CASE lastkey() = -4       && F5     12.05.2011
      @ 23,2 say 'Ждите! Идет формирование печати'
      set console off
      set device to print
      @prow(),1 say chr(27)+chr(33)+chr(4)
      @ prow()+1,1 say ' Перечень новых моделей                                                          '+dtoc(date())
      @ prow()+1,1 say '┌──────────┬──────────┬───┬────────────────────┬────────┬───────┬────────┬──────────┬───┬───┬─────────┐'
      @ prow()+1,1 say '│  Модель  │   Дата   │Мес│ Ф.И.О. технолога   │Техн. оп│Тех.пош│ ПЗЦ рас│ОТИЗ тр/зт│ГНС│Стн│ Дата ГНС│'
      @ prow()+1,1 say '└──────────┴──────────┴───┴────────────────────┴────────┴───────┴────────┴──────────┴───┴───┴─────────┘'
      nz=recno()
      go top
      DO WHILE .not. EOF()
         @ prow()+1,1 say ' '+mod+' '+dtoc(dat)+'  '+str(mes,2)+' '+fio+'     '+str(tex,1)+'       '+str(tex_p,1)+'       '+str(otiz_r,1)+'        '+str(otiz,1)+'       '+str(gns,1)+'    '+str(stan,1)+'  '+dtoc(datg)
         SKIP
      ENDDO
      @ prow(),1 say ' '
      set device to screen
      set console on
      go nz
      @ 23,1,23,78 box ramka3
      return(2)
   CASE lastkey()=22                     && INS-вставка новой записи
        if prz1=1
           DO WHILE .not. add_rec()
           ENDDO
           DO WHILE .not. rec_lock()
           ENDDO
           replace prz with 'Н'
           replace dat with date()
           unlock
         endif
           return(1)
   CASE lastkey()=7                      && DEL-удаление(восстановление) записи
        if prz1=1
           DO WHILE .not. rec_lock()
           ENDDO
           IF delete()
               recall record recno()
               replace prz with ' '
               flag_u=flag_u-1
           ELSE
               delete record recno()
               flag_u=flag_u+1
               replace prz with 'У'
           ENDIF
           unlock
        endif
        return(1)
   CASE lastkey() = 28                    && F1-помощь
        stcw=setcolor()
        save screen to scr
        DO HELP_US
        setcolor(stcw)
        restore screen from scr
        return(1)
   CASE lastkey()= -1                     &&F2 -быстрый поиск
        nzap=recno()
        r_mod=space(10)
        @23,1 clear to 23,78
        @23,5 say 'Введите модель ' get r_mod picture 'XXXXXXXXXX'
        read
        kl=alltrim(r_mod)
        set softseek ON
        find '&kl'
        IF .not. found()
           tone(400,3)
           @23,1 clear to 23,78
           @23,5 say ' B справочнике нет модели '+r_mod+'!'
           inkey(1)
           go nzap
        ENDIF
        set softseek OFF
        @23,1 clear to 23,78
*******блок для ГНС разрешает работать только в 10-м поле*******
        if prz1=3
           do case
              case i>=1 .and. i<=9
                   keyboard(replicate(chr(4),10-i))
              case i=12
                   keyboard(chr(19))
           endcase
         endif
********************************************************************
       return(1)
    OTHERWISE
        DO CASE
           case prz1=0
                if i = 11             && вносится иформация бюро цен
                   keyboard(chr(lastkey()))
                   row=row()
                   col=col()
                   DO WHILE .not. rec_lock()
                   ENDDO
                   @row,col get &cur_pole pict '@K' valid &cur_pole < 2
                   read
                endif
           case prz1=1                             && редактирование полей
                if (i > 1 .and. i < 9)         && вносится иформация тех бюро
                   keyboard(chr(lastkey()))
                   row=row()
                   col=col()
                   DO WHILE .not. rec_lock()
                   ENDDO
                   if i=3 .or.i=6 .or. i=7 .or. i=8
                      @row,col get &cur_pole pict '@K'
                   elseif i=2
                      @row,col get &cur_pole pict '@d' valid year(&cur_pole)>=2008
                   else
                      @row,col get &cur_pole pict '@K' valid &cur_pole < 2
                   endif
                   read
                   if i=7
                      if fio=space(20)
                         DO PFIO
                      endif
                   endif
                endif
           case prz1=2
                if i > 8 .and. i < 10       && вносится иформация отиза
                   keyboard(chr(lastkey()))
                   row=row()
                   col=col()
                   DO WHILE .not. rec_lock()
                   ENDDO
                   @row,col get &cur_pole pict '@K' valid &cur_pole < 2
                   read
                endif
           case prz1=3
                if i > 9 .and. i < 11        && вносится иформация ГНС
                   mr=mod
                   kl_p=mod
                   keyboard(chr(lastkey()))
                   row=row()
                   col=col()
                   DO WHILE .not. rec_lock()
                   ENDDO
                   @row,col get &cur_pole pict '@K' valid &cur_pole < 2
                   read
                   if gns=1
                      if datg=ctod(' ')             &&& 15.06.2010
                         replace datg with date()   &&& если ГНС=1 садится тек.дата в поле datg,если это поле '  '
                         select infgns                  &&& 26.08.2010
                         DO WHILE .not. add_rec()
                         ENDDO
                         DO WHILE .not. rec_lock()
                         ENDDO
                         replace datg with infmod->datg
                         replace mod with infmod->mod
                         unlock
                         select infmod                   &&& 26.08.2010
                      endif
                      do nesvm
                   endif
                   select polotno
                   find '&kl_p'
                   if found()
                      r_mod=mod
                      DO WHILE r_mod=mod .and. .not. eof()
                         DO WHILE .not. rec_lock()
                         ENDDO
                         replace dat with date()
                         unlock
                         skip
                       ENDDO
                    else
                      tone(500,5)
                    endif
                    select infmod
                endif
           case prz1=4
                if i > 10          && вносится иформация стадарт и НСИ
                   keyboard(chr(lastkey()))
                   row=row()
                   col=col()
                   DO WHILE .not. rec_lock()
                   ENDDO
                   @row,col get &cur_pole pict '@K' valid &cur_pole < 2
                   read
               endif
        ENDCASE
        DO CASE
           case prz='Н'
                replace prz with ' '
           case prz='Д'
                replace prz with ' '
        ENDCASE
        if i=3
           rc5=recno()
           kl_p=mod
           find '&kl_p'
           if found()
              if recno() <> rc5
                 go rc5
                 tone(300,3)
                 @row,col say 'ПОВТОР!   '
                 inkey(1)
                 replace mod with space(10)
              endif
           endif
        endif
        if i=3
           kl_po=mod
           select cenmod
           find '&kl_po'
           if found()
              tone(300,3)
              @row,col say 'ОБСЧИТАНА!   '
              inkey(1)
              replace infmod->mod with space(10)
           endif
           select infmod
        endif
        if prz1=1
           if i = 7
              keyboard repl(chr(19),5)
           else
              keyboard chr(4)
           endif
        else
           keyboard chr(4)
        endif
        unlock
        return(1)
ENDCASE
***********  Проверка заполнения справочников ГНС  ******************
PROCEDURE ERROR
select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod

COPY to rab_inf fields mod,prz

select 0
USE rab_inf EXCLUSIVE
index on mod to rab_inf
set index to rab_inf

DECLARE pole[2]
pole[1]='mod'
pole[2]='prz'

DECLARE shapka[2]
shapka[1]='Модель'
shapka[2]='Пометь!'

DO OKNO
@2,27 clear to 22,51
@2,27 to 22,51 double
@0,5 say '[ Проверка готовности модели ]'
@0,50 say '[ F2-Поиск модели ]'
@24,5 say '[ <Enter>-Пометить модель ]'
@24,58 say '[ <Esc>-Выход ]'
DBEDIT (4,30,21,50,pole,'f_vib','',shapka)
close all
release all
RETURN
*********************************************************************
FUNCTION f_vib
PARAMETERS mode,i
set cursor on
private cur_pole
cur_pole=pole[i]
DO CASE
   CASE mode < 4
        if i=1
           keyboard chr(4)             && куpсоp на 2 поле
        endif
        return(1)
   CASE lastkey()=13                    && RETURN-выйти и пометить модель
        replace prz with chr(251)
        keyboard chr(24)
        return(1)
   CASE lastkey()= -1                     &&F2 -быстрый поиск
        nzap=recno()
        r_mod=space(10)
        @23,1 clear to 23,78
        @23,5 say 'Введите модель ' get r_mod picture 'XXXXXXXXXX'
        read
        kl=alltrim(r_mod)
        set softseek ON
        find '&kl'
        IF .not. found()
           tone(400,3)
           @23,1 clear to 23,78
           @23,5 say ' B справочнике нет модели '+r_mod+'!'
           inkey(1)
           go nzap
        ENDIF
        set softseek OFF
        @23,1 clear to 23,78
        return(1)
   CASE lastkey()=27                    && ESC-выйти
        close all
        DO PEC_ERR
        return(0)
   OTHERWISE
        keyboard(chr(lastkey()))
        @row(),col() get &cur_pole pict '@K'
        read
        keyboard chr(24)
        return(1)
ENDCASE
*********************************************************************
PROC PEC_ERR
select 0
if .not. NET_USE('j:\texno\m101001','m101001',.f.)
  release all
  close all
  return
endif
set index to j:\texno\m101001

select 0
if .not. NET_USE('j:\nsi_nov\sprnop','sprnop',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprnop

select 0
if .not. NET_USE('j:\nsig\nsikis','nsikis',.f.)
  release all
  close all
  return
endif
set index to j:\nsig\nsikis

select 0
if .not. NET_USE('j:\nsi_nov\sprrvm','sprrvm',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprrvm4,j:\nsi_nov\sprrvm,j:\nsi_nov\sprrvm1,j:\nsi_nov\sprrvm2,j:\nsi_nov\sprrvm3

select 0
if .not. NET_USE('j:\nsi_nov\nsiskl','nsiskl',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\nsiskl1

select 0
create j:\nsi_nov\nsip\rab_gns
append blank
replace FIELD_NAME WITH 'ARTP1',field_type with 'C',field_len with 7
append blank
replace FIELD_NAME WITH 'ARTP2',field_type with 'C',field_len with 7
append blank
replace FIELD_NAME WITH 'NOR',field_type with 'N',field_len with 9,field_dec with 4
append blank
replace FIELD_NAME WITH 'OOB',field_type with 'N',field_len with 5,field_dec with 2
create j:\nsi_nov\nsip\rab_gns1 from j:\nsi_nov\nsip\rab_gns

if .not. NET_USE('j:\nsi_nov\nsip\rab_gns1','gns1',.t.)
  release all
  close all
  return
endif
index on artp1 to j:\nsi_nov\nsip\rab_gns1
set index to j:\nsi_nov\nsip\rab_gns1

select 0
create j:\nsi_nov\nsip\rab_gns2
append blank
replace FIELD_NAME WITH 'VM1',field_type with 'N',field_len with 4
append blank
replace FIELD_NAME WITH 'VD1',field_type with 'N',field_len with 2
append blank
replace FIELD_NAME WITH 'VM2',field_type with 'N',field_len with 4
append blank
replace FIELD_NAME WITH 'VD2',field_type with 'N',field_len with 2
append blank
replace FIELD_NAME WITH 'NOR',field_type with 'N',field_len with 7,field_dec with 3
create j:\nsi_nov\nsip\rab_gns3 from j:\nsi_nov\nsip\rab_gns2

if .not. NET_USE('j:\nsi_nov\nsip\rab_gns3','gns3',.t.)
  release all
  close all
  return
endif
index on str(vm1,4)+str(vd1,2) to j:\nsi_nov\nsip\rab_gns3
set index to j:\nsi_nov\nsip\rab_gns3

select 0
if .not. NET_USE('j:\nsi_nov\sprpot','po',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprpot

select 0
USE rab_inf alias rabinf EXCLUSIVE
set filter to prz=chr(251)
go top

DO GOTOV_LP
set console off
set device to print

nest=0
DO WHILE .not. eof()
   select gns1
   ZAP
   select gns3
   ZAP
   select rabinf
   p_mod=rabinf->mod
   select m101001
   find '&p_mod'
   if found() = .f.
      @ prow()+1,10 say 'У технологов нет данных по модели '+p_mod
      nest=1
      select rabinf
      SKIP
      loop
   endif
   for t = 1 to 25                       && стыковка всп.материалов у технологов и ГНС
     tt = str(t,len(alltrim(str(t))))
     if vm&tt <> 0 .and. vm&tt < 8000
       rvm = vm&tt
       rvd = vd&tt
       select gns3
       kl_t = str(rvm,4)+str(rvd,2)
       find '&kl_t'
       if found() = .f.
         append blank
         replace vm1 with rvm,vd1 with rvd
       endif
       select m101001
     endif
   next
   select sprrvm
   find '&p_mod'
   if found() = .f.
     @ prow()+1,10 say 'В спр.норм на всп. материалы нет данных по модели '+p_mod
      nest=1
      select rabinf
      SKIP
      loop
   endif
   do while .not. eof() .and. m = p_mod
     rkvm1 = kvm1
     rvd2 = kvd
     if nor = 0
       @ prow()+1,10 say 'В спр.норм на всп. материалы по модели '+alltrim(p_mod)+' ВМ '+alltrim(str(rkvm1,4))+' норма=0'
       nest=1
     endif
     if kvm1 = 0          &&&
       @ prow()+1,10 say 'В спр.норм на всп. материалы по модели '+alltrim(p_mod)+' код вспом. = 0'
       nest=1
     endif               &&&
     if kuda = 0
       @ prow()+1,10 say 'В спр.норм на всп. материалы по модели '+alltrim(p_mod)+' ВМ '+alltrim(str(rkvm1,4))+' пр.цеха = 0'
       nest=1
     endif
     if rkvm1 <> 0
        select gns3
        kl_g = str(rkvm1,4)+str(rvd2,2)
        find '&kl_g'
        if found() = .f.
           append blank
           replace vm1 with 0,vd1 with 0,vm2 with rkvm1,vd2 with rvd2
       else
           replace vm2 with rkvm1,vd2 with rvd2
       endif
       select sprrvm
     endif
     skip
   enddo
   select m101001            &&  стыковка артикулов полотен у технологов и ГНС
   for t = 1 to 10
     tt = str(t,len(alltrim(str(t))))
     if artp&tt <> space(7)
       rartp = artp&tt
       select gns1
       seek rartp
       if found() = .f.
          append blank
          replace artp1 with rartp
       endif
       select m101001
     endif
   next
   select sprnop
   find '&p_mod'
   if found() = .f.
      @ prow()+1,10 say 'В спр.норм расхода полотна и отходов на изделие нет данных по модели '+p_mod
      nest=1
      select rabinf
      SKIP
      loop
   endif
   do while .not. eof() .and. mod = p_mod
     rartp = arp
     if nor = 0 .or. oob = 0 .or. otx = 0
       @ prow()+1,10  say 'В спр.норм расхода полотна и отходов на изделие по модели '+alltrim(p_mod)+ ' по арт. '+alltrim(rartp)+' норма=0'
       nest=1
     endif
     if rartp <> space(7)
        select gns1
        seek rartp
       if found() = .f.
         append blank
         replace artp1 with space(7),artp2 with rartp
       else
         replace artp2 with rartp
       endif
       select sprnop
     endif
     skip
   enddo
   select gns1
   go top
   do while .not. eof()
     if artp1 <> artp2
       if artp1 <> space(7) .and. artp2 = space(7)
         @ prow()+1,10 say 'В спр.норм расхода полотна и отходов на изделие по мод. '+alltrim(p_mod)+' нет арт. '+artp1
         nest=1
       endif
       if artp1 = space(7) .and. artp2 <> space(7)
         @ prow()+1,10 say 'У технологов по мод. '+alltrim(p_mod)+' нет арт. '+artp2
         nest=1
       endif
     endif
     rartp2 = artp2
     select nsikis
     seek rartp2
     if found() =.f.
       @ prow()+1,10 say 'В спр. коэф. использования сырья нет арт. '+rartp2
       nest=1
     else
       if kgp = 0
         @ prow()+1,10 say 'В спр. коэф. использования сырья по арт. '+alltrim(rartp2)+' Коэф.= 0'
         nest=1
       endif
     endif
     select gns1
     skip
   enddo
   select gns3
   go top
   do while .not. eof()
     if vm1 <> vm2
       if vm1 <> 0 .and. vm2 = 0
         @ prow()+1,10 say 'В спр.норм на всп. материалы по мод. '+p_mod+' нет кода ВМ '+str(vm1,4)
         nest=1
       endif
      if vm1 = 0 .and. vm2 <> 0
         @ prow()+1,10 say 'У технологов по мод. '+p_mod+' нет кода ВМ '+str(vm2,4)
         nest=1
       endif
     endif
     if vd1 <> vd2 .and. vm1 = vm2
       @ prow()+1,10 say 'По мод. '+p_mod+' ВМ '+str(vm1,4)+' проверьте доп.код'
       nest=1
     endif
     skip
   enddo
   select sprrvm
   find '&p_mod'
   nul_nor = 1
   nest_skl = 0
   do while .not. eof() .and. p_mod = m
     rkvm = kvm1
     if ncn = 1
       skip
       loop
     endif
     store 0 to summa,k,summa_pr,ra0
     rra1=' '
     rro1=' '
     do while .not. eof() .and. p_mod = m .and. rkvm = kvm1
       rra = ra
       rro = ro
       if alltrim(rra) <> '0' .and. rra <> '   '
         rnor = nor
         if alltrim(rro) <> '0' .and. rro <> '   '
           kl1 = p_mod+rra+rro
           select nsiskl
           find '&kl1'
           if found()
             rprc = 0
             do while .not. eof() .and. p_mod = mdp .and. rra = raz .and. rro = rst
              rprc = rprc + prc
              skip
             enddo
             if rprc = 100
               k = 1
             endif
             if rra=rra1 .and. rro=rro1
              summa = summa + round(rnor*rprc,4)     &&& 05.04.12 округлила до 4 знаков после запятой (просьба ГНС)
             else
              summa_pr = summa_pr + rprc
              summa = summa + round(rnor*rprc,4)     &&& 05.04.12 округлила до 4 знаков после запятой (просьба ГНС)
             endif
             ra0 = 1
           else
             if nest_skl = 0
               @ prow()+1,10 say 'Нет шкалы по модели '+p_mod+' разм. '+rra+' росту '+rro+ ' КВМ='+str(rkvm,5)
               nest=1
               nest_skl = 1
             endif
           endif
         else
           kl1 = p_mod+rra
           select nsiskl
           find '&kl1'
           if found()
             rprc = 0
             do while .not. eof() .and. p_mod = mdp .and. rra = raz
              rprc = rprc + prc
              skip
             enddo
             if rprc = 100
               k = 1
             endif
             if rra=rra1
              summa = summa + round(rnor*rprc,4)
             else
              summa_pr = summa_pr + rprc
              summa = summa + round(rnor*rprc,4)
             endif
             ra0 = 1
           else
             if nest_skl = 0
               @ prow()+1,10 say 'Нет шкалы по модели '+p_mod+' размеру '+rra+ ' КВМ='+str(rkvm,5)
                nest=1
                nest_skl = 1
             endif
           endif
         endif
         select sprrvm
         rra1=rra
         rro1=rro
         skip
       else
         do while .not. eof() .and. p_mod = m .and. rkvm = kvm1 .and. rra = ra
           summa = summa + nor*100
           skip
         enddo
       endif
     enddo
     if round(summa_pr,4) <> 100 .and. ra0 = 1
       @ prow()+1,10 say 'По модели '+alltrim(p_mod)+ ' КВМ = '+alltrim(str(rkvm,4))+' в шкале размеров нет 100 %'
       nest=1
     endif
     if round(summa_pr,4) = 0 .and. ra0 = 1
       @ prow()+1,10 say 'По модели '+alltrim(p_mod)+ ' КВМ = '+alltrim(str(rkvm,4))+' в шкале размеров 0 %'
       nest=1
       nest_skl = 1
     endif
     select po
     seek rkvm
     if found()
       rkoef = koef
     else
       rkoef = 1
     endif
     if ra0 = 1
       if summa_pr <> 0 .and. rkoef <> 0
          sr_nor = round(summa/rkoef/summa_pr,4)
       else
         sr_nor = 0
       endif
     else
       sr_nor = round(summa/rkoef/100,4)
     endif
     if sr_nor <> 0
       nul_nor = 0
     else
       @ prow()+1,10 say 'Расчитанные нормы всп/мат. = 0 по модели '+p_mod
       nest=1
     endif
     select sprrvm
   enddo
   if nul_nor = 1 .and. nest_skl = 0
     @ prow()+1,10 say 'Расчитанные нормы всп/мат. = 0 по модели '+p_mod
     nest=1
   endif
   @ prow()+1,0 say ' '
   SELECT rabinf
   SKIP
ENDDO
if nest=1
  @ prow()+1,10 say '*************   '+dtoc(date())+'   ************'
  @ prow()+1,0 say ' '
endif
set console on
set device to screen
close all
release all
*erase rab_inf.dbf
*erase rab_inf.ntx
*erase j:\nsi_nov\nsip\rab_gns.dbf
*erase j:\nsi_nov\nsip\rab_gns.ntx
*erase j:\nsi_nov\nsip\rab_gns1.dbf
*erase j:\nsi_nov\nsip\rab_gns1.ntx
*erase j:\nsi_nov\nsip\rab_gns2.dbf
*erase j:\nsi_nov\nsip\rab_gns2.ntx
*erase j:\nsi_nov\nsip\rab_gns3.dbf
*erase j:\nsi_nov\nsip\rab_gns3.ntx
RETURN
**************  Проверка обсчитанных моделей  ************************
PROCEDURE ERROR2
select 0
IF .not. net_use('j:\nsig\cenmod','cenmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod,j:\nsig\cenmod1

COPY to rab_inf fields mod,prz

select 0
USE rab_inf EXCLUSIVE
index on mod to rab_inf
set index to rab_inf

DECLARE pole[2]
pole[1]='mod'
pole[2]='prz'

DECLARE shapka[2]
shapka[1]='Модель'
shapka[2]='Пометь!'

DO OKNO
@2,27 clear to 22,51
@2,27 to 22,51 double
@0,5 say '[ Проверка готовности модели ]'
@0,50 say '[ F2-Поиск модели ]'
@24,5 say '[ <Enter>-Пометить модель ]'
@24,58 say '[ <Esc>-Выход ]'
DBEDIT (4,30,21,50,pole,'f_vib1','',shapka)
close all
release all
RETURN
*********************************************************************
FUNCTION f_vib1
PARAMETERS mode,i
set cursor on
private cur_pole
cur_pole=pole[i]
DO CASE
   CASE mode < 4
        if i=1
           keyboard chr(4)             && куpсоp на 2 поле
        endif
        return(1)
   CASE lastkey()=13                    && RETURN-выйти и пометить модель
        replace prz with chr(251)
        keyboard chr(24)
        return(1)
   CASE lastkey()= -1                     &&F2 -быстрый поиск
        nzap=recno()
        r_mod=space(10)
        @23,1 clear to 23,78
        @23,5 say 'Введите модель ' get r_mod picture 'XXXXXXXXXX'
        read
        kl=alltrim(r_mod)
        set softseek ON
        find '&kl'
        IF .not. found()
           tone(400,3)
           @23,1 clear to 23,78
           @23,5 say ' B справочнике нет модели '+r_mod+'!'
           inkey(1)
           go nzap
        ENDIF
        set softseek OFF
        @23,1 clear to 23,78
        return(1)
   CASE lastkey()=27                    && ESC-выйти
        close all
        DO PEC_ERR
        return(0)
   OTHERWISE
        keyboard(chr(lastkey()))
        @row(),col() get &cur_pole pict '@K'
        read
        keyboard chr(24)
        return(1)
ENDCASE
**********************************************************************
PROC PFIO
save screen to ekran
SELECT fio
go top

DECLARE pole[2]
pole[1]='kodf'
pole[2]='fam'

DECLARE shapka[2]
shapka[1]='Код '
shapka[2]='Ф.И.О. технолга'

@2,55 clear to 22,76
@2,55 to 22,76 double
DBEDIT (3,57,21,75,pole,'f_p1','',shapka)
SELECT infmod
DO WHILE .not. rec_lock()
ENDDO
replace fio with r_fio
*unlock
restore screen from ekran
RETURN
***************** функция пользователя f_P1 ************************
FUNCTION f_p1
PARAMETERS mode,i
set cursor on
private cur_pole
cur_pole=pole[i]
DO CASE
   CASE mode < 4
        if i=1
           keyboard chr(4)
        endif
        return(1)
     CASE lastkey()=13                    && RETURN-выйти и посадить конкр пол
          r_fio=fam
          return(0)
     CASE lastkey()=27                    && ESC-выйти и посадить kod = 0
          r_fio='      '
          return(0)
     OTHERWISE
          return(1)
ENDCASE
*********************************************************************
PROC PEC_INF1        &&печать справочника
select 0
IF .not. net_use('j:\nsig\cenmod','cenmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod,j:\nsig\cenmod1

select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod

if pp=0
   set filter to tex=1 .and. tex_p=1 .and. otiz=1 .and. otiz_r=1 .and. gns=1 .and. stan=1
endif
go top

DO OKNO
@0,5 say '[ Формирование печати ]'
DO SOOB
lst=1
ks=68
pc=2
pcd=15
set device to print
set print on
set printer to f_infmod
set console off
setprc(0,0)
@prow(),1 say chr(27)+chr(33)+chr(4)
cherta=repl('-',92)
if pp=0
   @prow()+1,5 say 'Перечень моделей готовых для расчета цен             '+dtoc(date())
else
   @prow()+1,5 say 'Процесс отработки с новой моделью отделами           '+dtoc(date())
endif
@prow()+1,86 say 'стр.'+str(lst,2)
@prow()+1,1 say cherta
@prow()+1,1 say ': Модель  :   Дата   :Мес: Ф.И.О. технолога   :Т/отд оп:Т/отд п:ОТИЗ рас:ОТИЗ тр/зт:ГНС:Стн:'
@prow()+1,1 say cherta
DO WHILE .not. EOF()
   detstr=' '+mod+' '+dtoc(dat)+'  '+str(mes,2)+' '+fio+'     '+str(tex,1)+'       '+str(tex_p,1)+'       '+str(otiz_r,1)+'        '+str(otiz,1)+'       '+str(gns,1)+'    '+str(stan,1)
   @prow()+1,1 say detstr
   if prow() = ks
      DO S_SH
   endif
   if pp=0
      kl_p=mod
      select cenmod
      find '&kl_p'
      if .not. found()
          DO WHILE .not. add_rec()
          ENDDO
          DO WHILE .not. rec_lock()
          ENDDO
          replace mod with infmod->mod
          if dat=ctod(space(8))
             replace dat with date()
          else
             replace dat with infmod->dat
          endif
          replace tex with infmod->tex
          replace tex_p with infmod->tex_p
          replace otiz with infmod->otiz
          replace otiz_r with infmod->otiz_r
          replace gns with infmod->gns
          replace stan with infmod->stan
          replace mes with infmod->mes
          replace fio with infmod->fio
          unlock
      endif
      select infmod
      DO WHILE .not. rec_lock()
      ENDDO
      DELETE
      unlock
   endif
   SKIP
ENDDO
@prow()+2,26 say 'Конец инфоpмации'
@prow(),1 say ' '
set printer to
set print off
set device to screen
set console on
DO PEC with 'f_infmod'
set filter to
close all
RETURN
**********************************************************************
PROC PEC_INF2        &&печать справочника2
ud=0
DO OKNO
@0,5 say '[ Формирование печати ]'
@3,12 say '[ Введите период за котрый хотите получить информацию! ]'
n_dat=ctod(space(8))
k_dat=ctod(space(8))
@5,12 say '[ Начальная дата(чч.мм.гггг)-> ]' get n_dat
read
@7,12 say '[ Конечная дата(чч.мм.гггг)->  ]' get k_dat
read
if lastkey()=27
   close all
   release all
   RETURN
endif

select 0
IF .not. net_use('j:\nsig\cenmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod1
set filter to dat>=n_dat .and. dat<=k_dat
go top

lst=1
ks=68
pc=2
pcd=15
set device to print
set print on
set printer to f_infmdo
set console off
setprc(0,0)
@prow(),1 say chr(27)+chr(33)+chr(4)
cherta=repl('-',92)
@prow()+1,5 say 'Перечень моделей по которым рассчитаны цены             '+dtoc(date())
@prow()+1,86 say 'стр.'+str(lst,2)
@prow()+1,1 say cherta
@prow()+1,1 say ': Модель  :   Дата   :Мес: Ф.И.О. технолога   :Т/отд оп:Т/отд п:ОТИЗ рас:ОТИЗ тр/зт:ГНС:Стн:'
@prow()+1,1 say cherta
DO WHILE .not. EOF()
   detstr=' '+mod+' '+dtoc(dat)+'  '+str(mes,2)+' '+fio+'     '+str(tex,1)+'       '+str(tex_p,1)+'       '+str(otiz_r,1)+'        '+str(otiz,1)+'       '+str(gns,1)+'    '+str(stan,1)
   @prow()+1,1 say detstr
   if prow() = ks
      DO S_SH
   endif
   SKIP
ENDDO
@prow()+2,32 say 'Конец инфоpмации'
@prow(),1 say ' '
set printer to
set print off
set device to screen
set console on
DO PEC with 'f_infmdo'
set filter to
close all
RETURN
********************** проц. сокр. шапка ****************************
PROC S_SH
lst=lst+1
setprc(0,0)
@prow()+2,1 say '------------------------------- л и н и я   р а з р е з а ---------------------------------'
@prow()+1,86 say 'стр.'+str(lst,2)
@prow()+1,1 say cherta
@prow()+1,1 say ': Модель  :   Дата   :Мес: Ф.И.О. технолога   :Т/отд оп:Т/отд п:ОТИЗ рас:ОТИЗ тр/зт:ГНС:Стн:'
@prow()+1,1 say cherta
RETURN
************** ПРОЦЕДУРА УПАКОВКИ ***********************************
PROC UPK_INF1
DO OKNO
@0,5 say '[ Упаковка справочника ]'
DO SOOB
close all
select 0
IF .not. net_use('j:\nsig\infmod','infmod',.t.)
    close all
    release all
    RETURN
ENDIF
set index to j:\nsig\infmod
PACK

select 0
if .not. NET_USE('j:\nsi_nov\nesvm','nesvm',.t.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\nesvm,j:\nsi_nov\nesvm1
PACK
close all
RETURN
**************** ПРОЦЕДУРА индексации *******************************
PROC IND_INF1
DO OKNO
@0,5 say '[ Индексация справочника ]'
DO SOOB
close all
select 0
IF .not. net_use('j:\nsig\infmod','infmod',.t.)
    close all
    release all
    RETURN
ENDIF
index on mod to j:\nsig\infmod

select 0
IF .not. net_use('j:\nsig\cenmod','cenmod',.t.)
    close all
    release all
    RETURN
ENDIF
index on mod to j:\nsig\cenmod
index on dtos(dat)+mod to j:\nsig\cenmod1

select 0
if .not. NET_USE('j:\nsi_nov\nesvm','nesvm',.t.)
  release all
  close all
  return
endif
index on prz+mod+str(vmt,4)+str(vdt,2) to j:\nsi_nov\nesvm
index on prz+mod+str(vmg,4)+str(vdg,2) to j:\nsi_nov\nesvm1
close all
RETURN
**********************************************************************
PROC PRS_INF2           &&-просмотр просчитанных моделей
set exclusive OFF
select 0
IF .not. net_use('j:\nsig\cenmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod,j:\nsig\cenmod1

DECLARE pole[11]
pole[1]='prz'
pole[2]='dat'
pole[3]='mod'
pole[4]='tex'
pole[5]='tex_p'
pole[6]='mes'
pole[7]='fio'
pole[8]='otiz_r'
pole[9]='otiz'
pole[10]='gns'
pole[11]='stan'

DECLARE shapka[11]
shapka[1]='П'
shapka[2]='Дата'
shapka[3]='Модель'
shapka[4]=' техн;информ'
shapka[5]='техн; пош'
shapka[6]='Мес;плн'
shapka[7]=' Ф.И.О. ; технолога'
shapka[8]='ОТИЗ;раск'
shapka[9]='ОТИЗ;тр/з'
shapka[10]='ГНС'
shapka[11]='Ст'
flag_u=0
r_fio=''
DO OKNO
@24,5 say '[ F2-поиск модели ]'
DBEDIT (4,2,21,77,pole,'f_INF2','',shapka)
close all
release all
RETURN
***************** функция пользователя f_INF2 ***********************
FUNCTION f_INF2
PARAMETERS mode,i
set cursor on
private cur_pole
cur_pole=pole[i]
DO CASE
   CASE mode < 4
        if i=1
           keyboard chr(4)         && куpсоp на 2 поле
        endif
        @2,28 say ' Модель->[ '+mod+' ]'
        return(1)
   CASE lastkey()=27                    && ESC-выход из корректировки
        close all
        clear
        return(0)
   CASE lastkey()=22                     && INS-вставка новой записи
        DO WHILE .not. add_rec()
        ENDDO
        DO WHILE .not. rec_lock()
        ENDDO
        replace prz with 'Н'
        unlock
        return(1)
   CASE lastkey()=7                      && DEL-удаление(восстановление) записи
        DO WHILE .not. rec_lock()
        ENDDO
        IF delete()
            recall record recno()
            replace prz with ' '
            flag_u=flag_u-1
            keyboard chr(24)
        ELSE
            delete record recno()
            flag_u=flag_u+1
            replace prz with 'У'
            keyboard chr(24)
        ENDIF
        unlock
        return(1)
   CASE lastkey()= -1                     &&F2 -быстрый поиск
        nzap=recno()
        r_mod=space(10)
        @23,1 clear to 23,78
        @23,5 say 'Введите модель ' get r_mod picture 'XXXXXXXXXX'
        read
        kl=r_mod
        find '&kl'
        IF .not. found()
           tone(400,3)
           @23,1 clear to 23,78
           @23,5 say ' B справочнике нет модели '+r_mod+'!'
           inkey(1)
           go nzap
        ENDIF
        @23,1 clear to 23,78
        return(1)
    OTHERWISE
        IF i > 1                   && pазpeшена коppектиpовка начиная сo 2 поля
           keyboard(chr(lastkey()))
           row=row()
           col=col()
           DO WHILE .not. rec_lock()
           ENDDO
           @row,col get &cur_pole pict '@K'
           read
           replace prz with ' '
           if i=3
              rc2=recno()
              kl_p=mod
              find '&kl_p'
              if found()
                 if recno() <> rc2
                    go rc2
                    tone(300,3)
                    @row,col say 'ПОВТОР!'
                    inkey(1)
                    replace mod with '   '
                  endif
              endif
           endif
           if i=11
              keyboard repl(chr(19),10)
           else
              keyboard chr(4)
           endif
        unlock
        ENDIF
        return(1)
ENDCASE
*********************************************************************
PROC NESVM
kl3=' '+mr
sele nesvm
find '&kl3'
if found()=.t.
  do while.not.eof().and.' '=prz.and.mr=mod
    do while .not. rec_lock()
    enddo
    repl vmt with 0,vdt with 0,vmg with 0,vdg with 0,kodf with 0
    unlock
    skip
  enddo
endif
sele m101001
find '&kl_p'
if found()=.t.
  rkodf=kodf
  sele sprrvm
  find '&kl_p'  && по модели
  if found()=.t.
    do while.not.eof().and.mr=m
      vmr=kvm1
      vdr=kvd
      sele m101001
      store 0 to estt
      i=1
      for i=1 to 25
        vmtr='vm'+alltrim(str(i))
        vdtr='vd'+alltrim(str(i))
        if vmr=&vmtr .and. vdr=&vdtr
          estt=1
          exit
        endif
      next
      if estt=0
        DO VNEST
      endif
      sele sprrvm
      skip
    enddo
  endif
  sele m101001
  i=1
  for i=1 to 25
    store 0 to estt
    vmtrr='vm'+alltrim(str(i))
    vdtrr='vd'+alltrim(str(i))
    vmtr=&vmtrr
    vdtr=&vdtrr
    if vmtr<>0
      sele sprrvm
      kl2=mr+str(vmtr,4)
      sele sprrvm
      find '&kl2'
      if found()=.t.
        do while.not.eof().and.mr=m.and.vmtr=kvm1
          if vdtr=kvd
            estt=1
            exit
          endif
          skip
        enddo
        if estt=0
          DO VNESG
        endif
      else
        DO VNESG
      endif
    endif
    sele m101001
  next
else
  sele sprrvm
  find '&kl_p'  && по модели
  if found()=.t.
    do while.not.eof().and.mr=m
      vmr=kvm1
      vdr=kvd
      DO VNEST
      sele sprrvm
      skip
    enddo
  endif
endif
sele nesvm
go top
find '&kl3'
if found()=.t.
  do while.not.eof().and.' '=prz.and.mr=mod
    if vmt=0.and.vdt=0.and.vmg=0.and.vdg=0
      do while .not. rec_lock()
      enddo
      repl prz with 'У'
      delete
      unlock
    endif
    skip
  enddo
endif
return
*********************************************************************
PROC VNEST
zapis=0
kl2=' '+mr+str(vmr,4)+str(vdr,2)
sele nesvm
set order to 1   && поиск у техн
find '&kl2'
if found()=.f.
  find '&kl3'
  if found()=.t.
    do while.not.eof().and.' '=prz.and.mr=mod
      if vmt=0
        do while .not. rec_lock()
        enddo
        repl data with date(),wr with time()
        repl vmt with vmr,vdt with vdr,kodf with rkodf
        unlock
        zapis=1
      endif
      skip
    enddo
  endif
  if zapis=0
    do while.not.add_rec()
    enddo
    do while .not. rec_lock()
    enddo
    repl mod with mr,data with date(),wr with time()
    repl vmt with vmr,vdt with vdr,kodf with rkodf
    unlock
  endif
endif
return
*********************************************************************
PROC VNESG
sele sprvc
seek vmtr
if found()=.t.
  grpr=grp
  if grpr=20
    return
  endif
endif
zapis=0
kl2=' '+mr+str(vmtr,4)+str(vdtr,2)
sele nesvm
set order to 2    && поиск в ГНС
find '&kl2'
if found()=.f.
  find '&kl3'
  if found()=.t.
    do while.not.eof().and.' '=przg.and.mr=mod
      if vmg=0
        do while .not. rec_lock()
        enddo
        repl data with date(),wr with time()
        repl vmg with vmtr,vdg with vdtr
        unlock
        zapis=1
      endif
      skip
    enddo
  endif
  if zapis=0
    do while.not.add_rec()
    enddo
    do while .not. rec_lock()
    enddo
    repl mod with mr,data with date(),wr with time()
    repl vmg with vmtr,vdg with vdtr
    unlock
  endif
endif
return
*********************************************************************
PROC PNESVM
select 0
IF .not. net_use('j:\nsi_nov\sprvc','sprvc',.f.)
   close all
   release all
   RETURN
ENDIF
set index to j:\nsi_nov\sprvc

select 0
IF .not. net_use('j:\nsig\spropis','spropis',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\spropis

select 0
if .not. NET_USE('j:\nsi_nov\nesvm','nesvm',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\nesvm1,j:\nsi_nov\nesvm

@ 1,1 clear to 23,78
@ 1,1 say '            Нестыковка справочника SPRRVM c технологами        '
@ 3,20 say 'Нет у технологов:  Нет в спр. SPRRVM:'
@ 22,1 say 'F2 - поиск модели'
@ 23,1 say 'F3 - поиск отраб. модели'

DECLARE pole[8]
pole[1]='przg'
pole[2]='mod'
pole[3]='vmt'
pole[4]='vdt'
pole[5]='vmg'
pole[6]='vdg'
pole[7]='data'
pole[8]='wr'

DECLARE shapka[8]
shapka[1]='Пр.'
shapka[2]='Модель'
shapka[3]='Код ВМ'
shapka[4]='Код оп.'
shapka[5]='Код ВМ'
shapka[6]='Код оп.'
shapka[7]='Дата'
shapka[8]='Время'

DBEDIT (4,1,18,78,pole,'f_gr','',shapka)
RETURN
***************** функция пользователя f_pos ************************
FUNCTION f_gr
PARAMETERS mode,j
set cursor on
private cur_pole
cur_pole=pole[j]
DO CASE
   CASE mode < 4
     vmtr=vmt
     vdtr=vdt
     vmgr=vmg
     vdgr=vdg
     naimt=space(30)
     naimg=space(30)
     dnaimt=space(25)
     dnaimg=space(25)
     sele sprvc
     seek vmtr
     if found()=.t.
       naimt=nmat
     endif
     sele sprvc
     seek vmgr
     if found()=.t.
       naimg=nmat
     endif
     kl=str(vmtr,4)+str(vdtr,2)
     sele spropis
     find '&kl'
     if found()=.t.
       dnaimt=opis
     endif
     kl=str(vmgr,4)+str(vdgr,2)
     sele spropis
     find '&kl'
     if found()=.t.
       dnaimg=opis
     endif
     @ 20,10 say naimt
     @ 21,10 say dnaimt
     @ 20,45 say naimg
     @ 21,45 say dnaimg
     sele nesvm
     return(1)
   CASE lastkey()=27                    && ESC-выход
     return(0)
  CASE lastkey() = -1               &&  F2-Быстpый поиск
    mm = space(10)
    @ 23,28 say ' Введите модель:' get mm picture 'XXXXXXXXXX'
    read
    kl=' '+mm
    find '&kl'
    if found() = .f.
      @ 23,28 say ' В массиве нет такой модели    '
      tone(300,5)
      inkey(2)
      @ 23,28 clear to 23,75
      go top
    endif
    return(2)
   CASE lastkey() = -2               &&  F3-Быстpый поиск
    mm = space(10)
    @ 23,28 say ' Введите модель:' get mm picture 'XXXXXXXXXX'
    read
    kl='1'+mm
    find '&kl'
    if found() = .f.
      @ 23,28 say '  В массиве нет такой модели    '
      tone(300,5)
      inkey(2)
      @ 23,28 clear to 23,75
      go top
    endif
    return(2)
   OTHERWISE
     IF j = 1                   && pазpeшена коppектиpовка начиная сo 2 поля
       keyboard(chr(lastkey()))
       row=row()
       col=col()
       rrprz=przg
       DO WHILE .not. rec_lock()
       ENDDO
       @row,col get &cur_pole pict '@K'
       read
       rprz=przg
       if rprz='1'.or.rprz=' '
         replace przg with rprz
       else
         replace przg with rrprz
       endif
       unlock
       go top
     ENDIF
     return(2)
ENDCASE
******************************************* 10.09.2008 **************
PROC PEC_INF5        && печать справочника по планам
clear
store 0 to nmr
@1,10 say 'Укажите N месяца с которым будете работать' get nmr pict '99' valid nmr > 0 .and. nmr < 13
read
if lastkey()=27
   close all
   release all
   RETURN
endif

if nmr > 9
   nmr1=str(nmr,2)
else
   nmr1='0'+str(nmr,1)
endif

select 0
IF .not. net_use('j:\nsig\cenmod','cenmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod,j:\nsig\cenmod1

select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod
set filter to .not.(tex=1 .and. tex_p=1 .and. otiz=1 .and. otiz_r=1 .and. gns=1 .and. stan=1)
go top

im_f='j:\z401\m401f'+nmr1
im_fi='j:\z401\m401f'+nmr1+'.ntx'

im_fa='j:\z401\m401f'+nmr1+'.dbf'
if file("&im_fa")=.F.
   tone(400,3)
   @23,1 clear to 23,78
   @22,20 say '[ Плана на этот месяц еще нет!!! ]'
   inkey(5)
   close all
   release all
   RETURN
endif
SELECT 0
IF .not. net_use('&im_f','m401f',.f.)         && план
   close all
   release all
   RETURN
ENDIF

r_g=year(LupDate())+100
rdat=ctod(str(day(LupDate()),2)+'.'+str(month(LupDate()),2)+'.'+str(r_g,4))
*rdat := LupDate()
if rdat < date()-33
  DO OKNO
  @13,6 say ' Старый план. Создан '+dtoc(rdat)+'. Позвоните в ПЭО (3-27)!!'
  inkey(0)
  close all
  return
endif

DO OKNO
@0,5 say '[ Формирование печати ]'
DO SOOB
lst=1
ks=68
pc=2
pcd=15
set device to print
set print on
set printer to f_inf
set console off
setprc(0,0)
@prow(),1 say chr(27)+chr(33)+chr(4)
cherta=repl('-',92)
n1=''
DO WIB_MES with nmr,n1
@prow()+1,5 say 'Перечень новых моделей для плана на '+n1+' месяц         '+dtoc(date())
@prow()+1,86 say 'стр.'+str(lst,2)
@prow()+1,1 say cherta
@prow()+1,1 say ': Модель  :   Дата   :Мес: Ф.И.О. технолога   :Т/отд оп:Т/отд п:ОТИЗ рас:ОТИЗ тр/зт:ГНС:Стн:'
@prow()+1,1 say cherta
select m401f
DO WHILE .not. EOF()
  rmod=mod
  select cenmod
  seek rmod
  if found()
    select m401f
    skip
    loop
  endif
  select infmod
  seek rmod
  if found()
    @ prow()+1,1 say ' '+mod+' '+dtoc(dat)+'  '+str(mes,2)+' '+fio+'     '+str(tex,1)+'       '+str(tex_p,1)+'       '+str(otiz_r,1)+'        '+str(otiz,1)+'       '+str(gns,1)+'    '+str(stan,1)
  endif
  select m401f
  skip
ENDDO
@prow()+2,26 say 'Конец инфоpмации'
@prow(),1 say ' '
set printer to
set print off
set device to screen
set console on
DO PEC with 'f_inf'
set filter to
close all
RETURN
*********************************************************************
PROC COR_OTIZ           &&-корректировка         &&&& 29.09.2010
set exclusive OFF
select 0
IF .not. net_use('j:\texno\sprtx','fio',.f.)
   close all
   release all
   RETURN
endif
set index to j:\texno\sprtx

select 0
IF .not. net_use('j:\nsig\cenmod','cenmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\cenmod,j:\nsig\cenmod1

select 0
if .not. NET_USE('j:\texno\m101001','m101001',.f.)
  release all
  close all
  return
endif
set index to j:\texno\m101001

select 0
if .not. NET_USE('j:\nsi_nov\sprrvm','sprrvm',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprrvm4,j:\nsi_nov\sprrvm,j:\nsi_nov\sprrvm1,j:\nsi_nov\sprrvm2,j:\nsi_nov\sprrvm3

select 0
if .not. NET_USE('j:\nsi_nov\nesvm','nesvm',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\nesvm,j:\nsi_nov\nesvm1

select 0
IF .not. net_use('j:\nsi_nov\sprnop','polotno',.f.)
   close all
   release all
   return
endif
set index to j:\nsi_nov\sprnop,j:\nsi_nov\sprnop1,j:\nsi_nov\sprnop2

select 0
if .not. NET_USE('j:\nsi_nov\sprvc','sprvc',.f.)
  release all
  close all
  return
endif
set index to j:\nsi_nov\sprvc

select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod
set filter to (tex=1 .and. tex_p=1 .and. otiz_r=1 .and. gns=1)
go top

DECLARE pole[11]
pole[1]='prz'
pole[2]='dat'
pole[3]='mod'
pole[4]='tex'
pole[5]='tex_p'
pole[6]='otiz_r'
pole[7]='gns'
pole[8]='otiz'
pole[9]='stan'
pole[10]='mes'
pole[11]='fio'

DECLARE shapka[11]
shapka[1]='П'
shapka[2]='Дата'
shapka[3]='Модель'
shapka[4]=' техн;информ'
shapka[5]='техн; пош'
shapka[6]='ПЗЦ;раск'
shapka[7]='ГНС'
shapka[8]='ОТИЗ;тр/з'
shapka[9]='Ст'
shapka[10]='Мес;плн'
shapka[11]=' Ф.И.О. ; технолога'

r_fio=''
DO OKNO
@24,55 say ' F5 - Печать '
DBEDIT (4,2,21,77,pole,'f_OTI1','',shapka)
close all
release all
RETURN
******************** функция пользователя f_OTI1 *********************
FUNCTION f_OTI1
PARAMETERS mode,i
set cursor on
private cur_pole
cur_pole=pole[i]
DO CASE
   CASE mode < 4
        if i=1
           keyboard chr(4)         && куpсоp на 2 поле
        endif
        @2,10 say ' Дата: '+dtoc(dat)
        @2,40 say ' Модель: '+mod
        return(1)
   CASE lastkey()=27                    && ESC-выход из корректировки
        close all
        clear
        return(0)
   CASE lastkey() = 28                    && F1-помощь
        stcw=setcolor()
        save screen to scr
        DO HELP_US
        setcolor(stcw)
        restore screen from scr
        return(1)
   CASE lastkey()= -1                     &&F2 -быстрый поиск
        nzap=recno()
        r_mod=space(10)
        @23,1 clear to 23,78
        @23,5 say 'Введите модель ' get r_mod picture 'XXXXXXXXXX'
        read
        kl=alltrim(r_mod)
        set softseek ON
        find '&kl'
        IF .not. found()
           tone(400,3)
           @23,1 clear to 23,78
           @23,5 say ' B справочнике нет модели '+r_mod+'!'
           inkey(1)
           go nzap
        ENDIF
        set softseek OFF
        @23,1 clear to 23,78
       return(1)
    CASE lastkey() = -4       && F5
       @ 23,2 say 'Ждите! Идет формирование печати'
       set console off
       set device to print
       @prow(),1 say chr(27)+chr(33)+chr(4)
       @ prow()+1,1 say ' Перечень новых моделей                                                          '+dtoc(date())
       @ prow()+1,1 say '┌──────────┬──────────┬───┬────────────────────┬────────┬───────┬────────┬──────────┬───┬───┐'
       @ prow()+1,1 say '│  Модель  │   Дата   │Мес│ Ф.И.О. технолога   │Техн. оп│Тех.пош│ ПЗЦ рас│ОТИЗ тр/зт│ГНС│Стн│'
       @ prow()+1,1 say '└──────────┴──────────┴───┴────────────────────┴────────┴───────┴────────┴──────────┴───┴───┘'
       nz=recno()
       go top
       DO WHILE .not. EOF()
          @ prow()+1,1 say ' '+mod+' '+dtoc(dat)+'  '+str(mes,2)+' '+fio+'     '+str(tex,1)+'       '+str(tex_p,1)+'       '+str(otiz_r,1)+'        '+str(otiz,1)+'       '+str(gns,1)+'    '+str(stan,1)
          SKIP
       ENDDO
       @ prow(),1 say ' '
       set device to screen
       set console on
       go nz
       @ 23,1,23,78 box ramka3
       return(2)
    OTHERWISE
           IF prz1=2
                if i > 8 .and. i < 10       && вносится иформация отиза
                   keyboard(chr(lastkey()))
                   row=row()
                   col=col()
                   DO WHILE .not. rec_lock()
                   ENDDO
                   @row,col get &cur_pole pict '@K' valid &cur_pole < 2
                   read
                endif
           ENDIF
        DO CASE
           case prz='Н'
                replace prz with ' '
           case prz='Д'
                replace prz with ' '
        ENDCASE
        if i=3
           rc5=recno()
           kl_p=mod
           find '&kl_p'
           if found()
              if recno() <> rc5
                 go rc5
                 tone(300,3)
                 @row,col say 'ПОВТОР!   '
                 inkey(1)
                 replace mod with space(10)
              endif
           endif
        endif
        if i=3
           kl_po=mod
           select cenmod
           find '&kl_po'
           if found()
              tone(300,3)
              @row,col say 'ОБСЧИТАНА!   '
              inkey(1)
              replace infmod->mod with space(10)
           endif
           select infmod
        endif
        if prz1=1
           if i = 7
              keyboard repl(chr(19),5)
           else
              keyboard chr(4)
           endif
        else
           keyboard chr(4)
        endif
        unlock
        return(1)
ENDCASE
*********************************************************************
PROC MEN_PEC1
fl_p=0
store 0 to m4,n4
cherta=' '
DO WHILE .t.
   DO OKNO
   @ 10,11 clear to 13,50
   set wrap on
   m4=n4
   @ 11,12 prom ' Печать всего справочника            '
   @ 12,12 prom ' Печать справочника за опред.период  '
   menu to m8
   DO CASE
      case m8=1
           n8=m8
           DO PEC_INF1
      case m8=2
           n8=m8
           DO PEC_INF3
      case m8=0 .or. lastkey()=27
           exit
     ENDCASE
ENDDO
close all
release all
RETURN
*********************************************************************
PROC PEC_INF3        && печать справочника за период
ud=0
DO OKNO
@0,5 say '[ Формирование печати ]'
@3,12 say '[ Введите период за который хотите получить информацию! ]'
n_dat=ctod(space(8))
k_dat=ctod(space(8))
@5,12 say '[ Начальная дата(чч.мм.гггг)-> ]' get n_dat
read
@7,12 say '[ Конечная дата(чч.мм.гггг)->  ]' get k_dat
read
if lastkey()=27
   close all
   release all
   RETURN
endif

select 0
IF .not. net_use('j:\nsig\infmod','infmod',.f.)
   close all
   release all
   RETURN
endif
set index to j:\nsig\infmod
set filter to dat>=n_dat .and. dat<=k_dat
go top

lst=1
ks=68
pc=2
pcd=15
set device to print
set print on
set printer to f_infmod
set console off
setprc(0,0)
@prow(),1 say chr(27)+chr(33)+chr(4)
cherta=repl('-',102)
@prow()+1,5 say 'Процесс отработки с новой моделью отделами           '+dtoc(date())
@prow()+1,86 say 'стр.'+str(lst,2)
@prow()+1,1 say cherta
@prow()+1,1 say ': Модель  :   Дата   :Мес: Ф.И.О. технолога   :Т/отд оп:Т/отд п:ОТИЗ рас:ОТИЗ тр/зт:ГНС:Стн: Дата ГНС:'
@prow()+1,1 say cherta
DO WHILE .not. EOF()
   detstr=' '+mod+' '+dtoc(dat)+'  '+str(mes,2)+' '+fio+'     '+str(tex,1)+'       '+str(tex_p,1)+'       '+str(otiz_r,1)+'        '+str(otiz,1)+'       '+str(gns,1)+'    '+str(stan,1)+'  '+dtoc(datg)
   @prow()+1,1 say detstr
   if prow() = ks
      DO S_SH1
   endif
   SKIP
ENDDO
@prow()+2,26 say 'Конец инфоpмации'
@prow(),1 say ' '
set printer to
set print off
set device to screen
set console on
DO PEC with 'f_infmod'
set filter to
close all
RETURN
***************************** проц сокр. шапка ***********************
PROC S_SH1
lst=lst+1
setprc(0,0)
@prow()+2,1 say '------------------------------------ л и н и я   р а з р е з а ---------------------------------------'
@prow()+1,86 say 'стр.'+str(lst,2)
@prow()+1,1 say cherta
@prow()+1,1 say ': Модель  :   Дата   :Мес: Ф.И.О. технолога   :Т/отд оп:Т/отд п:ОТИЗ рас:ОТИЗ тр/зт:ГНС:Стн: Дата ГНС:'
@prow()+1,1 say cherta
RETURN
*************** конец модуля INF_MOD.prg ******************************
