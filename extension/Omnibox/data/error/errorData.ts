import { errorRawData } from "./errorRawData";

export class ErrorData {
  code: string;
  title: string;
  route: string;
  introduced: string;

  constructor(code: string, title: string, route: string, introduced: string) {
    this.code = code;
    this.title = title;
    this.route = route;
    this.introduced = introduced;
  }
}

export const errorData: ErrorData[] = errorRawData.map(
  ([code, title, route, introduced]) => new ErrorData(code, title, route, introduced)
);
