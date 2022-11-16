# repeater-bot

## Description:
* This bot sends back tesxts messages and stickers to the users. It can work in two modes: console and telegram (can be switched in config).

## Commands:
* /help - gives a description of the bot to the user
* /repeat - asks a number from 1 to 5 from the user, and starts to repeat messages with this number of times

## Installation
* Clone repository
* Rename "config-template.json" to "config.json"
* Add token of your bot to "config.json"
* By default bot works in telegram mode, if you want to turn on console mode then change mode in config to "console"
* Start the bot by command `stack run` in terminal opened in folder with project

## Structure:
Module Main reads config, parses it and uses function according to your settings in config.json. Module TelegramBot is used to run bot in telegram mode, it uses Telegram Bot API to get updates. Module ConsoleBot is used to run bot in console mode.

Besides Main contains functions to open and to close TXT-file for logging.

Handling of JSON-files and config is carried out using libraries "aeson".

"ReaderT Design Pattern" and "Handle Pattern" were used in the development of the code.