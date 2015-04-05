//
//  MainMenuController.swift
//  Digipostarkiv
//
//  Created by Frode on 19/01/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Foundation
import Cocoa

class MenuController: NSObject {
    
    @IBOutlet weak var statusMenu: NSMenu!
    var statusItem: NSStatusItem!
    var statusImage: NSImage!
    var statusImageActive: NSImage!
    
    
    @IBOutlet weak var loginWindowController: LoginWindowController!
    @IBOutlet weak var appDelegate: DigipostarkivAppDelegate!
    
    override func awakeFromNib() {
        statusItem = NSStatusBar.systemStatusBar().statusItemWithLength(20.0)
        statusItem!.menu = statusMenu
        let statusImageSize = NSSize(width: 15.2, height: 20.0)
        statusImage = NSImage(named: "digipost-black.png")
        statusImage?.size = statusImageSize
        statusImageActive = NSImage(named: "digipost-red.png")
        statusImageActive?.size = statusImageSize
        let altStatusImage = NSImage(named: "digipost-white.png")
        altStatusImage?.size = statusImageSize
        statusItem!.image = statusImage
        statusItem!.alternateImage = altStatusImage
        statusItem!.highlightMode = true
    }
    
    @IBAction func exitApp(sender: NSMenu) {
        hs_exit()
        NSApplication.sharedApplication().terminate(self)
    }
    
    @IBAction func openInFinder(sender: NSMenu) {
        let homePath = NSHomeDirectory().stringByAppendingString("/Digipostarkiv")
        NSWorkspace.sharedWorkspace().openFile(homePath)
    }
    
    @IBAction func manualSync(sender: NSMenu) {
        if (Sync.isLoggedIn()) {
            dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), {
                self.appDelegate.fullSync()
            })
        } else {
            appDelegate.stopSyncTimer()
            loginWindowController.showOauthLoginPage()
        }
    }
    @IBAction func openDigipostInBrowser(sender: AnyObject) {
        let digipostUrl = NSURL(string: "https://www.digipost.no/app/#/")
        NSWorkspace.sharedWorkspace().openURL(digipostUrl!)
    }
    
    @IBAction func startSync(sender: NSMenu) {
        appDelegate.startSyncTimer()
    }
    
    @IBAction func stopSync(sender: AnyObject) {
        appDelegate.stopSyncTimer()
    }
    
    override func validateMenuItem(menuItem: NSMenuItem) -> Bool {
        let started = appDelegate.isStarted()
        if (menuItem.action == "startSync:") {
            menuItem.hidden = started
            return !started
        } else if (menuItem.action == "stopSync:") {
            menuItem.hidden = !started
            return started
        } else {
            return true
        }
    }
    
    func showActiveIcon() {
        statusItem?.image = statusImageActive
    }
    
    func showStandardIcon() {
        statusItem?.image = statusImage
    }
}