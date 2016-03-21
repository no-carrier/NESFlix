asm6 nesflix_mmc5.asm temp.bin
copy /b mmc5_header.bin+temp.bin+temp.bin+temp.bin+temp.bin+mmc5.chr nesflix_mmc5.nes
del temp.bin
pause