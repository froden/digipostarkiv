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
    
    BOOL syncInProgress;
    BOOL presentLogin;
    BOOL loggedIn;
    long runNumber;
}

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSMenu *statusMenu;
@property (assign) IBOutlet WebView *webView;

- (IBAction)exitApp:(id)sender;

- (IBAction)login:(id)sender;

- (IBAction)sync:(id)sender;

- (NSString*)parseCode:(NSURL*)url;

- (void)digipostSync:(NSTimer*)timer;

@end
