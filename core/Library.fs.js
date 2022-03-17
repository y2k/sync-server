import { Record, Union } from "../client/src/.fable/fable-library.3.2.9/Types.js";
import { record_type, array_type, int32_type, bool_type, lambda_type, class_type, unit_type, union_type, string_type } from "../client/src/.fable/fable-library.3.2.9/Reflection.js";
import { FSharpResult$2 } from "../client/src/.fable/fable-library.3.2.9/Choice.js";
import { isNullOrEmpty } from "../client/src/.fable/fable-library.3.2.9/String.js";
import { ofArray, singleton } from "../client/src/.fable/fable-library.3.2.9/List.js";

export function Effect_call(_arg1) {
    throw (new Error("???"));
}

export class PreferencesEffect extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["PreferencesEffect"];
    }
}

export function PreferencesEffect$reflection() {
    return union_type("SyncServer.PreferencesEffect", [], PreferencesEffect, () => [[["Item1", string_type], ["Item2", string_type]]]);
}

export class UpdateModelEffect$1 extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["UpdateModelEffect"];
    }
}

export function UpdateModelEffect$1$reflection(gen0) {
    return union_type("SyncServer.UpdateModelEffect`1", [gen0], UpdateModelEffect$1, () => [[["Item", gen0]]]);
}

export class AddEffect$1 extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["AddEffect"];
    }
}

export function AddEffect$1$reflection(gen0) {
    return union_type("SyncServer.AddEffect`1", [gen0], AddEffect$1, () => [[["server", string_type], ["pass", string_type], ["title", string_type], ["url", string_type], ["Item5", lambda_type(union_type("Microsoft.FSharp.Core.FSharpResult`2", [unit_type, class_type("System.Exception")], FSharpResult$2, () => [[["ResultValue", unit_type]], [["ErrorValue", class_type("System.Exception")]]]), gen0)]]]);
}

export class ClientComponent_Model extends Record {
    constructor(serverHost, serverPass, title, url, isBusy, linkType, linkTypes) {
        super();
        this.serverHost = serverHost;
        this.serverPass = serverPass;
        this.title = title;
        this.url = url;
        this.isBusy = isBusy;
        this.linkType = (linkType | 0);
        this.linkTypes = linkTypes;
    }
}

export function ClientComponent_Model$reflection() {
    return record_type("SyncServer.ClientComponent.Model", [], ClientComponent_Model, () => [["serverHost", string_type], ["serverPass", string_type], ["title", string_type], ["url", string_type], ["isBusy", bool_type], ["linkType", int32_type], ["linkTypes", array_type(string_type)]]);
}

export function ClientComponent_Model__get_buttonDisabled(this$) {
    if (isNullOrEmpty(this$.url)) {
        return true;
    }
    else {
        return this$.isBusy;
    }
}

export function ClientComponent_Model__get_inputDisabled(this$) {
    return this$.isBusy;
}

export class ClientComponent_Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["ServerHostChanged", "UpdateConfiguration", "TitleChanged", "UrlChanged", "LinkTypeChanged", "Add", "AddResult"];
    }
}

export function ClientComponent_Msg$reflection() {
    return union_type("SyncServer.ClientComponent.Msg", [], ClientComponent_Msg, () => [[["Item", string_type]], [], [["Item", string_type]], [["Item", string_type]], [["Item", int32_type]], [], [["Item", union_type("Microsoft.FSharp.Core.FSharpResult`2", [unit_type, class_type("System.Exception")], FSharpResult$2, () => [[["ResultValue", unit_type]], [["ErrorValue", class_type("System.Exception")]]])]]]);
}

export const ClientComponent_init = new ClientComponent_Model("", "", "", "", false, 0, ["URL (bookmark)", "Watch late (youtube)"]);

export function ClientComponent_update(prefs, model, msg) {
    switch (msg.tag) {
        case 1: {
            return singleton(new PreferencesEffect(0, "server", model.serverHost));
        }
        case 2: {
            return singleton(new UpdateModelEffect$1(0, new ClientComponent_Model(model.serverHost, model.serverPass, msg.fields[0], model.url, model.isBusy, model.linkType, model.linkTypes)));
        }
        case 3: {
            return singleton(new UpdateModelEffect$1(0, new ClientComponent_Model(model.serverHost, model.serverPass, model.title, msg.fields[0], model.isBusy, model.linkType, model.linkTypes)));
        }
        case 4: {
            return singleton(new UpdateModelEffect$1(0, new ClientComponent_Model(model.serverHost, model.serverPass, model.title, model.url, model.isBusy, msg.fields[0], model.linkTypes)));
        }
        case 5: {
            return ofArray([new UpdateModelEffect$1(0, new ClientComponent_Model(model.serverHost, model.serverPass, model.title, model.url, true, model.linkType, model.linkTypes)), new AddEffect$1(0, model.serverHost, "FIXME", model.title, model.url, (arg0) => (new ClientComponent_Msg(6, arg0)))]);
        }
        case 6: {
            return singleton(new UpdateModelEffect$1(0, new ClientComponent_Model(model.serverHost, model.serverPass, "", "", false, 0, model.linkTypes)));
        }
        default: {
            return singleton(new UpdateModelEffect$1(0, new ClientComponent_Model(msg.fields[0], model.serverPass, model.title, model.url, model.isBusy, model.linkType, model.linkTypes)));
        }
    }
}

