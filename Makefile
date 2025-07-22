APPNAME = chordMapper
BINDIR = ./AppDir/usr/bin
USRDIR = ./AppDir/usr
BINFNAME = chordMapper-exe

.DEFAULT_GOAL := help

help:
	echo provide some arg

# wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
# chmod +x linuxdeploy-x86_64.AppImage

$(BINDIR):
	./linuxdeploy-x86_64.AppImage --appdir ./AppDir

build: $(BINDIR)
	stack build &&\
	stack install --local-bin-path=$(BINDIR)

$(USRDIR)/assets: 
	cp -r ./assets $(USRDIR)/

$(USRDIR)/config: 
	mkdir $(USRDIR)/config
	cp -p ./config/default.dhall $(USRDIR)/config/

run: $(BINDIR)/$(BINFNAME) $(USRDIR)/assets $(USRDIR)/config
	$(BINDIR)/$(BINFNAME) $(ARGS)

./AppDir/$(APPNAME).desktop:
	cp ./$(APPNAME).desktop ./AppDir/

./AppDir/chordMapper.png:
	cp ./imgs/chordMapper.png AppDir/

pack-linux: $(BINDIR)/$(BINFNAME) $(USRDIR)/assets $(USRDIR)/config ./AppDir/$(APPNAME).desktop ./AppDir/chordMapper.png
	./linuxdeploy-x86_64.AppImage --appdir ./AppDir --output appimage