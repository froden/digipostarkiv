//
//  AppDelegate.h
//  DPSync
//
//  Created by Frode Nerbråten on 22.04.14.
//  Copyright (c) 2014 Nerbraten. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface AppDelegate : NSObject <NSApplicationDelegate> {
    NSStatusItem *statusItem;
    NSTimer *syncTimer;
    
    BOOL syncInProgress;
    long runNumber;
}

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSMenu *statusMenu;
@property (assign) IBOutlet WebView *webView;

- (IBAction)exitApp:(id)sender;

- (IBAction)login:(id)sender;

- (IBAction)openArchiveFolder:(id)sender;

- (IBAction)sync:(id)sender;

- (NSString*)parseCode:(NSURL*)url;

- (void)digipostSync:(NSTimer*)timer;

- (void)stopSyncTimer;
- (void)startSyncTimer;

@end
