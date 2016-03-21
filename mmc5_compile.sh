./asm6 nesflix_mmc5.asm temp.bin
cat mmc5_header.bin temp.bin temp.bin temp.bin temp.bin mmc5.chr > nesflix_mmc5.nes
rm temp.bin
