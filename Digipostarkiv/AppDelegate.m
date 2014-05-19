//
//  AppDelegate.m
//  DPSync
//
//  Created by Frode Nerbråten on 22.04.14.
//  Copyright (c) 2014 Nerbraten. All rights reserved.
//

#import "AppDelegate.h"
#import "HsCocoa_stub.h"

@implementation AppDelegate

- (void)applicationWillFinishLaunching:(NSNotification *)aNotification {
    [[NSAppleEventManager sharedAppleEventManager] setEventHandler:self andSelector:@selector(handleURLEvent:withReplyEvent:) forEventClass:kInternetEventClass andEventID:kAEGetURL];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    presentLogin = true;
    syncInProgress = false;
    loggedIn = false;
    
    statusItem = [[NSStatusBar systemStatusBar] statusItemWithLength:NSVariableStatusItemLength];
    [statusItem setMenu:_statusMenu];
    NSImage *statusImage = [NSImage imageNamed:@"digipostbw.png"];
    [statusImage setSize:NSMakeSize(20.0, 20.0)];
    NSImage *altStatusImage = [NSImage imageNamed:@"digipostbw-white.png"];
    [altStatusImage setSize:NSMakeSize(20.0, 20.0)];
    [statusItem setImage:statusImage];
    [statusItem setAlternateImage:altStatusImage];
    [statusItem setHighlightMode:YES];
    
    [[NSTimer scheduledTimerWithTimeInterval:10.0
                                     target:self
                                   selector:@selector(digipostSync:)
                                   userInfo:nil
                                    repeats:true] fire];
}

- (IBAction)exitApp:(id)sender {
    hs_exit();
    [NSApp terminate:self];
}

- (IBAction)login:(id)sender {
    char *cAuthUrl = authUrl("state");
    NSString *authUrl = [NSString stringWithFormat:@"%s" , cAuthUrl];
    NSLog(@"%@", authUrl);
    NSURL *url = [NSURL URLWithString:authUrl];
    NSURLRequest *urlRequest = [NSURLRequest requestWithURL:url];
    [NSApp activateIgnoringOtherApps:YES];
    [self.window makeKeyAndOrderFront:self];
    [[self.webView mainFrame] loadRequest:urlRequest];
}

- (IBAction)logout:(id)sender {
    logout();
    loggedIn = false;
}

- (void)handleURLEvent:(NSAppleEventDescriptor*)event withReplyEvent:(NSAppleEventDescriptor*)replyEvent
{
    NSString *urlString = [[event paramDescriptorForKeyword:keyDirectObject] stringValue];
    NSURL *url = [NSURL URLWithString:urlString];
    NSLog(@"%@", url);
    NSString *authCode = [self parseCode:url];
    int result = accessToken("state", (char*)[authCode UTF8String]);
    NSLog(@"%i", result);
    
    if (result == 0) {
        loggedIn = true;
        [self.window close];
    }
}

- (NSString*)parseCode:(NSURL*)url {
    NSString* authCode;
    NSArray* urlComponents = [[url query] componentsSeparatedByString:@"&"];
    for (NSString *keyValuePair in urlComponents) {
        NSArray *pairComponents = [keyValuePair componentsSeparatedByString:@"="];
        if ([pairComponents count] == 2) {
            NSString *key = [pairComponents objectAtIndex:0];
            NSString *value = [pairComponents objectAtIndex:1];
            
            if ([key isEqualToString:@"code"]) {
                authCode = value;
            }
        }
    }

    return authCode;
}

- (IBAction)sync:(id)sender {
    [self performSelectorInBackground:@selector(digipostSync:) withObject:false];
}

- (BOOL)validateMenuItem:(NSMenuItem *)item {
    SEL action = [item action];
    if (action == @selector(login:)) {
        [item setHidden:loggedIn];
        return !loggedIn;
    } else if (action == @selector(logout:)) {
        [item setHidden:!loggedIn];
        return loggedIn;
    } else {
        return YES;
    }
}



- (void)digipostSync:(NSTimer*)timer {
    if (syncInProgress) {
        return;
    }
    syncInProgress = true;
    int result = syncNow();
    NSLog(@"Syncresult: %i", result);
    if (result == 0) {
        loggedIn = true;
    } else if (result == 1) {
        loggedIn = false;
        if (presentLogin) {
            presentLogin = false;
            [self performSelectorOnMainThread:@selector(login:) withObject:false waitUntilDone:false];
        }
    }
    syncInProgress = false;
}

@end
