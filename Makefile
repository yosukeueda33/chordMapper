APPNAME = chordMapper
BINDIR = ./AppDir/usr/bin
USRDIR = ./AppDir/usr
BINFNAME = chordMapper-exe
.DEFAULT_GOAL := help

help:
	echo provide some arg

# wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
# chmod +x linuxdeploy-x86_64.AppImage
LINUXDEPLOY = ./linuxdeploy-x86_64.AppImage
PACKITEMS = ./pack_items
PACKLINUXOBJS =  $(BINDIR)/$(BINFNAME) $(USRDIR)/assets $(USRDIR)/config ./AppDir/$(APPNAME).desktop \
			./AppDir/chordMapper.png ./AppDir/AppRun

$(BINDIR):
	$(LINUXDEPLOY) --appdir ./AppDir

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
	cp $(PACKITEMS)/$(APPNAME).desktop ./AppDir/

./AppDir/AppRun:
	cp $(PACKITEMS)/AppRun ./AppDir/

./AppDir/chordMapper.png:
	cp ./imgs/chordMapper.png AppDir/

pack-linux: $(PACKLINUXOBJS)
	$(LINUXDEPLOY) --appdir ./AppDir --output appimage