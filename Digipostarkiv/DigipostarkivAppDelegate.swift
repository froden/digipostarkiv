//
//  DigipostarkivAppDelegate.swift
//  Digipostarkiv
//
//  Created by Frode on 02/02/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Cocoa

class DigipostarkivAppDelegate: NSObject, NSApplicationDelegate {
    
    @IBOutlet weak var loginWindowController: LoginWindowController!
    @IBOutlet weak var menuController: MenuController!
    
    var syncInProgress: Bool
    var syncTimer: NSTimer?
    var runNumber = 0
    
    override init() {
        syncInProgress = false
        syncTimer = nil
    }
    
    func applicationWillFinishLaunching(notification: NSNotification) {
        NSAppleEventManager.sharedAppleEventManager().setEventHandler(
            self,
            andSelector: "handleURLEvent:replyEvent:",
            forEventClass: AEEventClass(kInternetEventClass),
            andEventID: AEEventID(kAEGetURL))
    }
    
    func applicationDidFinishLaunching(notification: NSNotification) {
        startSyncTimer()
    }
    
    func startSyncTimer() {
        if (syncTimer == nil || !syncTimer!.valid) {
            NSLog("starting sync timer")
            syncTimer = NSTimer.scheduledTimerWithTimeInterval(10.0, target: self, selector: "sync", userInfo: nil, repeats: true)
        }
        syncTimer!.fire()
    }
    
    func stopSyncTimer() {
        NSLog("stopping sync timer")
        if (syncTimer != nil && syncTimer!.valid) {
            syncTimer!.invalidate()
            syncTimer = nil
        }
    }
    
    func sync() {
        if (Sync.isLoggedIn()) {
            dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), {
                self.detectChangeAndSync()
            })
        } else {
            stopSyncTimer()
            loginWindowController.showOauthLoginPage()
        }
    }
    
    func detectChangeAndSync() {
        if (syncInProgress) {
            return;
        }
        syncInProgress = true;
        let checkRemote = runNumber++ % 6 == 0
        var remoteSync = false
        if (checkRemote) {
            let remoteChangeResult = Sync.hasRemoteChange()
            if (remoteChangeResult == 0) {
                remoteSync = true
                NSLog("Remote change detected")
            } else if (remoteChangeResult == 1) {
                dispatch_async(dispatch_get_main_queue(), {self.loginWindowController.showOauthLoginPage()})
                return
            } else if (remoteChangeResult == 99){
                NSLog("Unhandled syncresult from hsRemoteChange")
            }
        }
        let localSync = !checkRemote && Sync.hasLocalChange()
        if localSync {
            NSLog("Local change detected")
        }
        if (localSync || remoteSync) {
            fullSync()
        }
        syncInProgress = false;
    }
    
    func fullSync() {
        menuController.showActiveIcon()
        let result = Sync.sync();
        if (result != 0) {
            if (result == 1) {
                dispatch_async(dispatch_get_main_queue(), {self.loginWindowController.showOauthLoginPage()})
            } else {
                //TODO: give up after n failures?
                NSLog("Unhandled syncresult: %i", result);
            }
        }
        menuController.showStandardIcon()
    }
    
    func handleURLEvent(event: NSAppleEventDescriptor, replyEvent: NSAppleEventDescriptor) {
        
        let urlString = event.paramDescriptorForKeyword(AEKeyword(keyDirectObject))?.stringValue
        let url = urlString.flatMap { NSURL(string: $0) }
        let authCode = url.flatMap(parseCode)
        
        if let code = authCode {
            let result = Sync.getAccessToken("state", code)
            if (result == 0) {
                startSyncTimer()
                loginWindowController.closeOauthLoginPage()
            } else {
                NSLog("Error from getAccessToken: %i", result)
            }
        } else {
            NSLog("No auth code found in url")
        }
        
    }
    
    func parseCode(url: NSURL) -> NSString? {
        let urlComponents = url.query?.componentsSeparatedByString("&")
        let codeCompoents = urlComponents?
            .map{$0.componentsSeparatedByString("=")}
            .filter{$0.count == 2 && $0[0] == "code"}.first
        return codeCompoents?[1]
    }
    
}