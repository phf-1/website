/**
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */


/**
 * A Check is a function Any → Undefined such that if a value x does not verify some
 * invariant, then an error is raised.
 *
 * For instance, given point : Check, then point(x) = undefined means that x is a
 * point /i.e./ has been built using a Point constructor.
 *
 * [[id:fb8e2f12-4097-4490-93e9-439d2da24b17]]
 */
class Check {
  static mk(pred, msg) { return new Check(pred, msg); }

  #pred; #msg;
  constructor(pred, msg) {
    this.#pred = pred
    this.#msg = msg
    const callable = (x) => {
      if(pred(x)) { return undefined; }
      throw new TypeError(msg + `. x = ${x}`);
    }
    Object.setPrototypeOf(callable, Check.prototype);
    return callable
  }

  get pred() { return this.#pred; }
  get msg() { return this.#msg; }

  static use(proc) { return (check) => proc(check.pred, check.msg) }
}

Check.Symbol = Check.mk((x) => typeof x === 'symbol', 'not a Symbol')
Check.String = Check.mk((x) => typeof x === 'string' || x instanceof String, 'not a String')
Check.Integer = Check.mk((x) => Number.isInteger(x), 'not a Integer')
Check.HTMLElement = Check.mk((x) => x instanceof HTMLElement, 'not a HTMLElement')
Check.Any = Check.mk((_x) => true, '')
Check.Check = Check.mk((x) => x instanceof Check, 'not a Check')
Check.Unexpected = Check.mk((_x) => false, 'Unexpected value')


/**
 * Let I be an invariant. `I(x) = false' means that the invariant I is not satisfied
 * by x but should be. We want the calling program to be notified of this fact. So,
 * an Error2 is built and thrown to that end.
 *
 * For instance, if I(x) :≡ x > 0 and I(0) is executed, then err : Error2 is built
 * capturing relevant data and then thrown.
 *
 * [[id:2dbe8366-e235-49c8-83b7-ddeb3780b1a5]]
 */
class Error2 extends Error {
  static mk(code, msg, value = undefined) { return new Error2(code, msg, value); }

  #code; #msg; #value;
  constructor(code, msg, value = undefined) {
    Check.Symbol(code)
    Check.String(msg)
    Check.Any(value)
    super(msg);
    if (Error.captureStackTrace) { Error.captureStackTrace(this, Err); }
    this.#code = code;
    this.#msg = msg;
    this.#value = value;
  }

  get code()   { return this.#code; }
  get msg()    { return this.#msg; }
  get value()  { return this.#value; }
  get stack()  { return super.stack; }

  static use(proc) {
    return (err) => {
      Check.Error2(err)
      return proc(err.code, err.msg, err.value, err.stack);
    }
  }
}

Check.Error2 = Check.mk((x) => x instanceof Error2, 'Not an Error2')


/**
 * Given id : String, then return the first HTMLElement in the current document or
 * undefined.
 *
 * [[id:24ba1999-b5f5-4581-ad65-57040f553669]]
 */
const find_by_id = (id) => {
  Check.String(id)
  const results = document.querySelectorAll(`#${id}`)
  if (results.length !== 1) { return undefined; }
  const result = results.item(0)
  Check.HTMLElement(result)
  return result
}


/**
 * A Message represents an immutable piece of information sent from one actor to
 * another.
 *
 * For instance, "hello".
 *
 * [[id:764ae761-12e0-47c0-a39b-93b02a91b564]]
 */
class Message {
  static mk(code, value = undefined) { return new Message(code, value); }

  #code; #value;
  constructor(code, value = undefined) {
    Check.Symbol(code)
    Check.Any(value)
    this.#code = code;
    this.#value = value;
  }

  get code()   { return this.#code; }
  get value()  { return this.#value; }

  static use(proc) {
    return (message) => {
      Check.Message(message)
      return proc(message.code, message.value);
    }
  }
}

Check.Message = Check.mk((x) => x instanceof Message, 'Not a Message')

const define_message_class = (name, valueChecker = null) => {
  const code = Symbol(name);

  class M extends Message {
    static #code = code;

    constructor(value = undefined) {
      if (valueChecker) valueChecker(value);
      super(code, value);
    }
  }

  if (valueChecker) {
    M.mk = (value) => new M(value);
  } else {
    M.mk = () => new M();
  }

  Check[name] = Check.mk(x => x instanceof M, `Not a ${name}`);
  return M;
};

const Ping = define_message_class('Ping');
const Pong = define_message_class('Pong');
const Html = define_message_class('Html');
const Show = define_message_class('Show');
const Subscribe = define_message_class('Subscribe', Check.Actor);
const Unsubscribe = define_message_class('Unsubscribe', Check.Actor);
const Adopt = define_message_class('Adopt', Check.HTMLElement);
const Warning = define_message_class('Warning', Check.String);
const Info = define_message_class('Info', Check.String);
const Debug = define_message_class('Debug', Check.String);
const ErrorMessage = define_message_class('ErrorMessage', Check.String);


/**
 * A Just represents a present value, wrapping any value to indicate presence.
 *
 * For instance, Just("hello") represents the present string "hello".
 *
 * [[id:9aae292a-b43b-44d8-a08f-d10d61dbb29e]]
 */
class Just {
  static mk(value) { return new Just(value); }

  #value;
  constructor(value) {
    Check.Any(value)
    this.#value = value;
  }

  get value() { return this.#value; }

  static use(proc) {
    return (just) => {
      Check.Just(just);
      return proc(just.value);
    }
  }
}

Check.Just = Check.mk((x) => x instanceof Just, 'Not a Just')


/**
 * A Nothing represents the absence of a value.
 *
 * For instance, Nothing.mk().
 *
 * [[id:08e50354-a62a-4790-9ebb-e076c304b2a1]]
 */
class Nothing {
  static mk() { return new Nothing(); }

  constructor() {}

  static use(constant) {
    return (nothing) => {
      Check.Nothing(nothing);
      return constant;
    }
  }
}

Check.Nothing = Check.mk((x) => x instanceof Nothing, 'Not a Nothing')


/**
 * Maybe is the optional type formed by the union of Nothing and Just(X).
 *
 * For instance:
 *   Maybe.use(0, (v) => v + 10)(Just.mk(5))   // returns 15
 *   Maybe.use(0, (v) => v + 10)(Nothing.mk()) // returns 0
 *
 * [[id:34b6a43b-a408-46f6-a28e-52c70ce66f96]]
 */
const Maybe = {
  use(default_value, proc) {
    return (maybe) => {
      Check.Maybe(maybe);
      if (maybe instanceof Nothing) {
        return default_value;
      }
      return proc(maybe.value);
    }
  }
};

Check.Maybe = Check.mk(
  (x) => x instanceof Nothing || x instanceof Just,
  'Not a Maybe'
);


/**
 * An Actor is a stateful entity that processes messages, updates its state, and may
 * broadcast a signal to subscribed actors.
 *
 * For instance, with an appropriate tx, we may define clock :≡ Actor.mk(0, tx) so
 * that clock#tick#read = 1.
 *
 * [[id:63874e71-cb3e-4445-bc70-f054d347b7a7]]
 */
class Actor {
  static mk(state, tx) { return new Actor(state, tx); }

  #state;
  #tx;
  #subscribers = new Set();

  constructor(state, tx) {
    Check.Any(state);
    Check.Any(tx);
    this.#state = state;
    this.#tx = tx;
  }

  async rcv(msg) {
    Check.Message(msg);

    if (msg instanceof Ping) { return Pong.mk(); }
    if (msg instanceof Subscribe) { this.#subscribers.add(msg.value); return this; }
    if (msg instanceof Unsubscribe) { this.#subscribers.delete(msg.value); return this; }

    const [reply, next_state, signal] = await this.#tx(this.#state, msg);
    this.#state = next_state;

    if (signal instanceof Just) {
      const broadcast_msg = signal.value;
      for (const sub of this.#subscribers) { sub.rcv(broadcast_msg); }
    }

    return reply;
  }

  static use(msg) {
    return (actor) => {
      Check.Actor(actor);
      return actor.rcv(msg);
    }
  }

  subscribe(actor) { return this.rcv(Subscribe.mk(actor)); }
  unsubscribe(actor) { return this.rcv(Unsubscribe.mk(actor)); }
}

Check.Actor = Check.mk((x) => x instanceof Actor, 'Not an Actor')


/**
 * A Logger is an actor that logs messages it receives.
 *
 * For instance, logger#error("x > 1")
 *
 * [[id:cfcb07f8-5390-4c47-9f4e-53554fca76cf]]
 */
class Logger extends Actor {
  static mk() { return new Logger(); }

  constructor() {
    const nothing = Nothing.mk();
    const tx = async (state, msg) => {
      if (msg instanceof Debug) {
        console.log(`DEBUG | ${msg.value}`);
      }
      else if (msg instanceof Info) {
        console.log(`INFO | ${msg.value}`);
      }
      else if (msg instanceof Warning) {
        console.warn(`WARNING | ${msg.value}`);
      }
      else if (msg instanceof ErrorMessage) {
        console.error(`ERROR | ${msg.value}`);
      }
      else {
        Check.Unexpected(msg);
      }
      return [this, state, nothing];
    }
    super({}, tx);
  }

  debug(msg) { return this.rcv(Debug.mk(msg)); }
  info(msg) { return this.rcv(Info.mk(msg)); }
  warning(msg) { return this.rcv(Warning.mk(msg)); }
  error(msg) { return this.rcv(ErrorMessage.mk(msg)); }
}


const logger = Logger.mk()


/**
 * A Toc actor manages the table-of-contents HTMLElement and provides access to it.
 *
 * For instance, Toc.mk().html() returns the element with id 'table-of-contents'.
 *
 * [[id:cb9a99f9-17fa-47d3-9f3b-9191350e11be]]
 */
class Toc extends Actor {
  static mk() { return new Toc(); }

  constructor() {
    const nothing = Nothing.mk();
    const result = find_by_id('table-of-contents')
    Check.HTMLElement(result)
    const tx = async (state, msg) => {
      const { html } = state
      let reply = this;
      if (msg instanceof Html) { reply = html; }
      else { Check.Unexpected(msg); }
      return [reply, state, nothing];
    }
    super({ html: result }, tx);
  }

  html() { return this.rcv(Html.mk()); }
}


/**
 * A LastEdit actor manages the last-edit HTMLElement (if present) and provides access to it.
 *
 * For instance, LastEdit.mk().html() returns the element with id 'last-edit'.
 *
 * [[id:2ef2a775-eb16-4b6a-a997-dc54453c4f16]]
 */
class LastEdit extends Actor {
  static mk() { return new LastEdit(); }

  constructor() {
    const nothing = Nothing.mk();
    const result = find_by_id('last-edit')
    Check.HTMLElement(result)
    const tx = async (state, msg) => {
      const { html } = state
      let reply = this;
      if (msg instanceof Html) { reply = html; }
      else { Check.Unexpected(msg); }
      return [reply, state, nothing];
    }
    super({ html: result }, tx);
  }

  html() { return this.rcv(Html.mk()); }
}


/**
 * A Status actor manages the status HTMLElement (if present) and provides access to it.
 *
 * For instance, Status.mk().html() returns the element with id 'status'.
 *
 * [[id:60b273dc-fe99-4e8b-a6d9-04a85cb771ae]]
 */
class Status extends Actor {
  static mk() { return new Status(); }

  constructor() {
    const nothing = Nothing.mk();
    const result = find_by_id('status')
    Check.HTMLElement(result)
    const tx = async (state, msg) => {
      const { html } = state
      let reply = this;
      if (msg instanceof Html) { reply = html; }
      else { Check.Unexpected(msg); }
      return [reply, state, nothing];
    }
    super({ html: result }, tx);
  }

  html() { return this.rcv(Html.mk()); }
}


/**
 * A Cell actor manages a specific HTMLElement by id, allowing child adoption and html queries.
 *
 * For instance, Cell.mk('c21').adopt(child) appends the child element.
 *
 * [[id:60b273dc-fe99-4e8b-a6d9-04a85cb771ae]]
 */
class Cell extends Actor {
  static mk(id) { return new Cell(id); }

  constructor(id) {
    Check.String(id)
    const nothing = Nothing.mk();
    const result = find_by_id(id)
    Check.HTMLElement(result)
    const tx = async (state, msg) => {
      const { html } = state
      let reply = this;
      if (msg instanceof Html) { reply = html; }
      else if (msg instanceof Adopt) { html.appendChild(msg.value); }
      else { Check.Unexpected(msg); }
      return [reply, state, nothing];
    }
    super({ html: result }, tx);
  }

  adopt(html) { return this.rcv(Adopt.mk(html)); }
  html() { return this.rcv(Html.mk()); }
}


/**
 * A C21 actor is the Cell managing the HTMLElement with id 'c21'.
 *
 * [[id:b5da62a1-0f66-4d16-9a51-e776b25df269]]
 */
class C21 extends Cell {
  static mk() { return new C21(); }
  constructor() { super('c21'); }
}


/**
 * A C23 actor is the Cell managing the HTMLElement with id 'c23'.
 *
 * [[id:d5512f3e-6692-4ac7-abd3-6ab076be3d08]]
 */
class C23 extends Cell {
  static mk() { return new C23(); }
  constructor() { super('c23'); }
}

/**
 * An Article actor coordinates the page layout by placing components (TOC, last-edit, status)
 * into the appropriate cells and responds to Show messages.
 *
 * For instance, await Article.mk() builds the structure, and article.show() activates it on load.
 *
 * [[id:2f320f4c-1f91-469a-927f-c74db83851fb]]
 */
class Article extends Actor {
  static async mk() {
    const nothing = Nothing.mk();
    const toc = Toc.mk()
    const html = find_by_id('article')
    const c21 = C21.mk()
    const c23 = C23.mk()
    c21.adopt(await toc.html())

    try {
      const last_edit = LastEdit.mk()
      c23.adopt(await last_edit.html())
    }
    catch(_err) {
      // ignore
    }

    try {
      const status = Status.mk()
      c23.adopt(await status.html())
    }
    catch(_err) {
      // ignore
    }

    return new Article();
  }

  constructor() {
    const nothing = Nothing.mk();
    const tx = async (state, msg) => {
      if (msg instanceof Show) {
        document.documentElement.classList.remove("loading");
      }
      return [this, state, nothing]
    };
    super({}, tx);
  }

  show() { return this.rcv(Show.mk()); }
}

/**
 * Initializes the Article actor when the DOM is ready and triggers its show action on window load.
 */
(function(){
  let article = undefined;
  window.addEventListener('DOMContentLoaded', async () => { article = await Article.mk() });
  window.addEventListener('load', () => { article.show(); });
})();
