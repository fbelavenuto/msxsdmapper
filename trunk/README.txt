Projeto SD Mapper 512K para MSX

Copyright (c) 2014
Fabio Belavenuto
Licenced under
CERN OHL v1.1
http://ohwr.org/cernohl

This documentation describes Open Hardware and is licensed under the CERN OHL v. 1.1.
You may redistribute and modify this documentation under the terms of the
CERN OHL v.1.1. (http://ohwr.org/cernohl). This documentation is distributed
WITHOUT ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING OF MERCHANTABILITY,
SATISFACTORY QUALITY AND FITNESS FOR A PARTICULAR PURPOSE.
Please see the CERN OHL v.1.1 for applicable conditions


  Este projeto implementa uma interface em forma de cartucho para adicionar um
dispositivo de armazenamento em massa utilizando at� dois cart�es SD (Secure
Digital) e uma Mapper de 512K para computadores da linha MSX.
  Foi utilizado como sistema operacional o Nextor [1] sendo desenvolvido somente
o driver para se ter acesso aos dois cart�es SD. O Nextor � um projeto fechado,
sendo permitido o desenvolvimento de um driver open-source.
  O Nextor � uma evolu��o do MSX-DOS 2 tendo nativamente suporte a parti��es FAT16
de at� 4GB, podendo ter 4 parti��es por cart�o SD. � necess�rio 128KB de Mapper no
m�nimo para utilizar o Nextor com acesso � sub-diret�rios, menos que 128K de mapper
permite somente o uso do kernel do MSXDOS1 limitando em parti��es com FAT12 e
m�ximo de 32MB por parti��o.
  A Mapper pode ser desativada por uma chave e � �til no caso de usar a interface
em um MSX que j� tenha Mapper.
  Para compilar o driver, utilize o cross-compiler SJASM [2] e para compilar o 
c�digo do CPLD utilize o Xilinx ISE Webpack [3].
  Detalhes t�cnicos:
  - O c�digo do CPLD implementa toda a l�gica necess�ria, implementando um expansor
    de slots padr�o, uma porta SPI modo 0, o controle da MegaROM padr�o ASCII16
    utilizada pelo Nextor e o controle da Mapper de 512K.
  - Ao ativar a mapper, a interface ativa o expansor de slots, e com isso a
    interface funciona somente em slots n�o-expandidos. Por�m, se desativar a
    mapper, o expansor de slots � desativado, permitindo utilizar o dispositivo de
    armazenamento em massa em slots expandidos.
  - A porta SPI implementada utiliza as portas de I/O n�mero 8 e 9, limitando o uso
    de somente uma interface no MSX. Duas ou mais interfaces ligadas no MSX gerar�
    conflito.
  - Foi previsto a atualiza��o da mem�ria flash por software. Para isto foi
    implementado a porta 0xB7 que controla a ativa��o ou n�o da grava��o da flash.
    O bit 7 controla a permiss�o de grava��o e os bits 2, 1 e 0 controlam a pagina��o
    de 16KB. Ao ativar o bit 7 a flash � mapeada em qualquer p�gina (repete o mesmo
    espa�o de 16K nas 4 p�ginas) e os bits 2, 1 e 0 controlam qual das 8 p�ginas da
    flash ser� mapeada. O sinal /WR da flash s� � gerado quando o bit 7 est� ativo.



[1] http://www.konamiman.com/msx/msx-e.html#nextor
[2] http://home.wanadoo.nl/smastijn/sjasm.html
[3] http://www.xilinx.com/products/design-tools/ise-design-suite/ise-webpack.htm
