//
//  AppDelegate.h
//  DPSync
//
//  Created by Frode Nerbråten on 22.04.14.
//  Copyright (c) 2014 Nerbraten. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#import "Digipostarkiv-Swift.h"

@interface AppDelegate : NSObject <NSApplicationDelegate> {
    NSStatusItem *statusItem;
    NSImage *statusImage;
    NSImage *statusImageActive;
    NSTimer *syncTimer;
    
    MenuController *menuController;
    
    BOOL syncInProgress;
    long runNumber;
}

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSMenu *statusMenu;
@property (assign) IBOutlet WebView *webView;

- (IBAction)manualSync:(id)sender;

- (NSString*)parseCode:(NSURL*)url;

- (void)sync;
- (void)detectChangeAndSync:(NSTimer*)timer;

- (void)stopSyncTimer;
- (void)startSyncTimer;
- (void)fullSync;

@end
