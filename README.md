# repeater-bot

## Description:
* This bot sends back tesxts messages and stickers to the users. It can work in two modes: consol and telegram (can be switched in config).

## Commands:
/help - gives a description of the bot to the user
/repeat - asks a number from 1 to 5 from the user, and starts to repeat messages with this number of times

## Installation
* Rename "config-template.json" to "config.json"
* Add token of your bot to "config.json"
* By default bot works in telegram mode, if you want to turn on consol mode then change mode in config to "consol"

## Structure:
Module Main reads config, parses it and uses function according to your settings in config.json. Module TelegramBot is used to run bot in telegram mode, it uses Telegram Bot Api to get updates