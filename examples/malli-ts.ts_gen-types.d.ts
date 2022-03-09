import * as crypto from 'crypto';

/**
 * @schema [:=> [:catn [:s string?]] any?]
 */
export var k: (s:string) => any;
/**
 * @schema [:=> [:catn [:s string?]] any?]
 */
export var sym: (s:string) => any;
/**
 * @schema [:=> [:catn [:o any?]] any?]
 */
export var toClj: (o:any) => any;
/**
 * @schema [:=> [:catn [:schema any?] [:val any?]] any?]
 */
export var validate: (schema:any, val:any) => any;
/**
 * @schema [:=> :cat [any? {:external-type {:t-name "Date", :t-path nil, :t-alias nil}}]]
 */
export var now: () => Date;
/**
 * @schema [:=> [:catn [:s string?]] :crypto/hash]
 */
export var toSha256: (s:string) => crypto.Hash;